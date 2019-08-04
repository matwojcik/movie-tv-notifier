package matwojcik.movies

import java.util.concurrent.Executors

import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects
import au.id.tmm.bfect.effects.Bracket
import au.id.tmm.bfect.ziointerop._
import cats.effect.ConcurrentEffect
import cats.effect.ContextShift
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.Timer
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.typesafe.scalalogging.StrictLogging
import matwojcik.movies.bfect.BifunctorParallel
import matwojcik.movies.config.Config
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.FilmwebClient
import matwojcik.movies.mailing.Mails
import matwojcik.movies.recommendation.RecommendationRouter
import matwojcik.movies.recommendation.RecommendationSender
import matwojcik.movies.recommendation.RecommendationTemplating
import matwojcik.movies.recommendation.Recommendations
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder
import pureconfig.loadConfigOrThrow
import scalaz.zio.interop.ParIO
import scalaz.zio.interop.catz._
import scalaz.zio.DefaultRuntime
import scalaz.zio.ZIO

import scala.concurrent.ExecutionContext

object App extends scalaz.zio.App with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit val runtime: scalaz.zio.Runtime[Environment] = new DefaultRuntime {}

  type Effect[F[+_, +_], A] = F[Throwable, A]

  override def run(args: List[String]) =
    program[ZIO[Any, +?, +?], ParIO[Any, +?, +?]].fold(_ => 1, _ => 0)

  def program[F[+_, +_]: effects.Async: Bracket: effects.Timer, G[+_, +_]](
    implicit P: BifunctorParallel[F, G],
    CE: ConcurrentEffect[Effect[F, ?]],
    CS: ContextShift[Effect[F, ?]]
  ): F[Throwable, Unit] =
    appResource[F, G].use(_ => effects.Async[F].never)

  private def appResource[F[+_, +_]: effects.Sync: Bracket: effects.Timer, G[+_, +_]](
    implicit P: BifunctorParallel[F, G],
    CE: ConcurrentEffect[Effect[F, ?]],
    CS: ContextShift[Effect[F, ?]]
  ): Resource[Effect[F, ?], Unit] =
    for {
      sttpBackend <- Resource.make[Effect[F, ?], SttpBackend[Effect[F, ?], Nothing]](
        effects.Sync[F].syncThrowable(AsyncHttpClientCatsBackend[Effect[F, ?]]())
      )(backend => effects.Sync[F].syncThrowable(backend.close()))
      implicit0(config: Config) <- Resource.liftF(effects.Sync[F].syncThrowable(loadConfigOrThrow[Config]))
      service                   <- Resource.liftF(service[F, G](sttpBackend))
      _                         <- server[Effect[F, ?]](service)
    } yield ()

  private def service[F[+_, +_]: effects.Sync: effects.Bracket, G[+_, +_]](
    sttpBackend: SttpBackend[Effect[F, ?], Nothing]
  )(implicit P: BifunctorParallel[F, G],
    config: Config
  ) =
    for {
      implicit0(filmweb: Filmweb[F]) <- effects
        .Sync[F]
        .syncThrowable(Filmweb.instance[F](new FilmwebClient[F](config.filmweb, sttpBackend)))
      implicit0(mails: Mails[F])                         <- effects.Sync[F].pure(Mails.instance[F](config.mail))
      implicit0(recommendations: Recommendations[F])     <- effects.Sync[F].pure(Recommendations.instance[F, G])
      implicit0(templating: RecommendationTemplating[F]) <- effects.Sync[F].pure(RecommendationTemplating.instance[F])
      implicit0(sender: RecommendationSender[F])         <- effects.Sync[F].pure(RecommendationSender.instance[F](config.mail))
    } yield new MoviesRouter[F].service <+> new RecommendationRouter[F].service

  private def server[F[_]: Sync: ConcurrentEffect: Timer](routes: HttpRoutes[F])(implicit config: Config): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(config.http.port, config.http.host)
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}

