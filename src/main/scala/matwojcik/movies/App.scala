package matwojcik.movies

import java.util.concurrent.Executors

import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects.Sync._
import au.id.tmm.bfect.effects.Async
import au.id.tmm.bfect.effects.Bracket
import au.id.tmm.bfect.effects.Sync
import au.id.tmm.bfect.effects.Timer
import au.id.tmm.bfect.ziointerop._
import cats.effect.ConcurrentEffect
import cats.effect.ContextShift
import cats.effect.Resource
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.typesafe.scalalogging.StrictLogging
import matwojcik.movies.bfect.BifunctorParallel
import matwojcik.movies.bfect.implicits._
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
import scalaz.zio.DefaultRuntime
import scalaz.zio.ZIO
import scalaz.zio.interop.ParIO
import scalaz.zio.interop.catz._

import scala.concurrent.ExecutionContext

object App extends scalaz.zio.App with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit val runtime: scalaz.zio.Runtime[Environment] = new DefaultRuntime {}

  type WideEffect[F[+_, +_], A] = F[Throwable, A]

  override def run(args: List[String]) =
    program[ZIO[Any, +?, +?], ParIO[Any, +?, +?]].fold(_ => 1, _ => 0)

  def program[F[+_, +_]: Async: Bracket: Timer, G[+_, +_]](
    implicit P: BifunctorParallel[F, G],
    CE: ConcurrentEffect[WideEffect[F, ?]],
    CS: ContextShift[WideEffect[F, ?]]
  ): F[Throwable, Unit] =
    appResource[F, G].use(_ => Async[F].never)

  private def appResource[F[+_, +_]: Sync: Bracket: Timer, G[+_, +_]](
    implicit P: BifunctorParallel[F, G],
    CE: ConcurrentEffect[WideEffect[F, ?]],
    CS: ContextShift[WideEffect[F, ?]]
  ): Resource[WideEffect[F, ?], Unit] =
    for {
      sttpBackend <- Resource.make[WideEffect[F, ?], SttpBackend[WideEffect[F, ?], Nothing]](
        Sync[F].syncThrowable(AsyncHttpClientCatsBackend[WideEffect[F, ?]]())
      )(backend => Sync[F].syncThrowable(backend.close()))
      implicit0(config: Config) <- Resource.liftF(Sync[F].syncThrowable(loadConfigOrThrow[Config]))
      service                   <- Resource.liftF(service[F, G](sttpBackend))
      _                         <- server[WideEffect[F, ?]](service)
    } yield ()

  private def service[F[+_, +_]: Sync: Bracket, G[+_, +_]](
    sttpBackend: SttpBackend[WideEffect[F, ?], Nothing]
  )(implicit P: BifunctorParallel[F, G],
    config: Config
  ) =
    for {
      client                                             <- Sync[F].syncThrowable(new FilmwebClient[F](config.filmweb, sttpBackend))
      implicit0(filmweb: Filmweb[F])                     <- Filmweb.instance[F](client).biPure[F]
      implicit0(mails: Mails[F])                         <- Mails.instance[F](config.mail).biPure[F]
      implicit0(recommendations: Recommendations[F])     <- Recommendations.instance[F, G].biPure[F]
      implicit0(templating: RecommendationTemplating[F]) <- RecommendationTemplating.instance[F].biPure[F]
      implicit0(sender: RecommendationSender[F])         <- RecommendationSender.instance[F](config.mail).biPure[F]
    } yield new MoviesRouter[F].service <+> new RecommendationRouter[F].service

  private def server[F[_]: cats.effect.Sync: ConcurrentEffect: cats.effect.Timer](
    routes: HttpRoutes[F]
  )(implicit config: Config
  ): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(config.http.port, config.http.host)
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}
