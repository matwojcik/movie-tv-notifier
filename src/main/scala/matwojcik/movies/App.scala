package matwojcik.movies

import java.util.concurrent.Executors

import cats.{Defer, Parallel}
import cats.effect.{Async, ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, LiftIO, Resource, Sync, Timer}
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.typesafe.scalalogging.StrictLogging
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import matwojcik.movies.config.Config
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.FilmwebClient
import matwojcik.movies.filmweb.FilmwebConfig
import matwojcik.movies.recommendation.{RecommendationRouter, Recommendations}
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder
import pureconfig.loadConfigOrThrow

import scala.concurrent.ExecutionContext

object App extends IOApp with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit def unsafeLogger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  override def run(args: List[String]): IO[ExitCode] = program[IO, IO.Par].as(ExitCode.Success)

  def program[F[_]: Async: ConcurrentEffect: Timer: ContextShift, G[_]](implicit P: Parallel[F,G]): F[Unit] =
    appResource[F,G].use(_ => Async[F].never[Unit])

  private def appResource[F[_]: Sync: ConcurrentEffect: LiftIO: Timer: ContextShift, G[_]](implicit P: Parallel[F,G]): Resource[F, Unit] =
    for {
      sttpBackend <- Resource.make(Sync[F].delay(AsyncHttpClientCatsBackend[F]()))(backend => Sync[F].delay(backend.close()))
      implicit0(config: Config) <- Resource.liftF(LiftIO[F].liftIO(IO(loadConfigOrThrow[Config])))
      service     <- Resource.liftF(service(sttpBackend))
      _           <- server[F](service)
    } yield ()

  private def service[F[_]: Sync, G[_]](sttpBackend: SttpBackend[F, Nothing])(implicit P: Parallel[F,G], config: Config) =
    for {
      implicit0(filmweb: Filmweb[F]) <- Filmweb.instance[F](new FilmwebClient[F](config.filmweb, sttpBackend)).pure[F]
      implicit0(recommendations: Recommendations[F]) <- Recommendations.instance[F,G].pure[F]
    } yield new MoviesRouter[F].service <+> new RecommendationRouter[F].service

  private def server[F[_]: Sync: ConcurrentEffect: Timer](routes: HttpRoutes[F]): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(5000, "localhost")
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}
