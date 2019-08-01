package matwojcik.movies

import java.util.concurrent.Executors

import cats.Parallel
import cats.effect._
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.asynchttpclient.cats.AsyncHttpClientCatsBackend
import com.typesafe.scalalogging.StrictLogging
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
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

import scala.concurrent.ExecutionContext

object App extends IOApp with StrictLogging {

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  implicit def unsafeLogger[F[_]: Sync]: Logger[F] = Slf4jLogger.getLogger[F]

  override def run(args: List[String]): IO[ExitCode] = program[IO, IO.Par].as(ExitCode.Success)

  def program[F[_]: Async: ConcurrentEffect: Timer: ContextShift, G[_]](implicit P: Parallel[F, G]): F[Unit] =
    appResource[F, G].use(_ => Async[F].never[Unit])

  private def appResource[F[_]: Sync: ConcurrentEffect: Timer: ContextShift, G[_]](implicit P: Parallel[F, G]): Resource[F, Unit] =
    for {
      sttpBackend               <- Resource.make(Sync[F].delay(AsyncHttpClientCatsBackend[F]()))(backend => Sync[F].delay(backend.close()))
      implicit0(config: Config) <- Resource.liftF(Sync[F].delay(loadConfigOrThrow[Config]))
      service                   <- Resource.liftF(service(sttpBackend))
      _                         <- server[F](service)
    } yield ()

  private def service[F[_]: Sync, G[_]](sttpBackend: SttpBackend[F, Nothing])(implicit P: Parallel[F, G], config: Config) =
    for {
      implicit0(filmweb: Filmweb[F])                     <- Filmweb.instance[F](new FilmwebClient[F](config.filmweb, sttpBackend)).pure[F]
      implicit0(mails: Mails[F])                         <- Mails.instance[F](config.mail).pure[F]
      implicit0(recommendations: Recommendations[F])     <- Recommendations.instance[F, G].pure[F]
      implicit0(templating: RecommendationTemplating[F]) <- RecommendationTemplating.instance[F].pure[F]
      implicit0(sender: RecommendationSender[F])         <- RecommendationSender.instance[F](config.mail).pure[F]
    } yield new MoviesRouter[F].service <+> new RecommendationRouter[F].service

  private def server[F[_]: Sync: ConcurrentEffect: Timer](routes: HttpRoutes[F])(implicit config: Config): Resource[F, Server[F]] =
    BlazeServerBuilder[F]
      .bindHttp(config.http.port, config.http.host)
      .withHttpApp(Router("/" -> routes).orNotFound)
      .resource

}
