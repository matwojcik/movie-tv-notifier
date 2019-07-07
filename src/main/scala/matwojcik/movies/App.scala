package matwojcik.movies

import java.util.concurrent.Executors

import cats.effect.Async
import cats.effect.ConcurrentEffect
import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.Timer
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.Router
import org.http4s.server.Server

import scala.concurrent.ExecutionContext

object App extends IOApp with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit def unsafeLogger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  override def run(args: List[String]): IO[ExitCode] = program[IO].as(ExitCode.Success)

  def program[F[_]: Async: ConcurrentEffect: Timer]: F[Unit] =
    server[F](new MoviesRouter[F].service).use(_ => Async[F].never[Unit])

  private def server[F[_]: Sync: ConcurrentEffect: Timer](routes: HttpRoutes[F]): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(8080, "localhost")
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}
