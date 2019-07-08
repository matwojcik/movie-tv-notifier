package matwojcik.movies

import java.util.concurrent.Executors

import cats.effect.Async
import cats.effect.ConcurrentEffect
import cats.effect.ContextShift
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.Timer
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.typesafe.scalalogging.StrictLogging
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.FilmwebClient
import matwojcik.movies.filmweb.FilmwebConfig
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

object App extends IOApp with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit def unsafeLogger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  val config = FilmwebConfig("https://ssl.filmweb.pl/api", "apikey")

  override def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)

  def program[F[_]: Async: ConcurrentEffect: Timer: ContextShift]: F[Unit] =
    appResource[F].use(_ => Async[F].never[Unit])

  private def appResource[F[_]: Sync: ConcurrentEffect: Timer: ContextShift]: Resource[F, Unit] =
    for {
      sttpBackend <- Resource.make(Sync[F].delay(AsyncHttpClientCatsBackend[F]()))(backend => Sync[F].delay(backend.close()))
      service     <- Resource.liftF(service(sttpBackend))
      _           <- server[F](service)
    } yield ()

  private def service[F[_]: Sync](sttpBackend: SttpBackend[F, Nothing]) =
    for {
      implicit0(filmweb: Filmweb[F]) <- Filmweb.instance[F](new FilmwebClient[F](config, sttpBackend)).pure[F]
    } yield new MoviesRouter[F].service

  private def server[F[_]: Sync: ConcurrentEffect: Timer](routes: HttpRoutes[F]): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}
