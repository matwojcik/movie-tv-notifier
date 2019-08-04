package matwojcik.movies.recommendation

import java.time.LocalDate
import java.time.ZoneId

import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects._
import cats.syntax.all._
import matwojcik.movies.filmweb.domain.Channel
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._

class RecommendationRouter[F[+_, +_]: Sync: Bracket: Recommendations: RecommendationTemplating: RecommendationSender]
  extends Http4sDsl[F[Throwable, ?]] {

  val zone = ZoneId.of("Europe/Warsaw")

  val service: HttpRoutes[F[Throwable, ?]] = HttpRoutes.of[F[Throwable, ?]] {
    case GET -> Root / "recommendations" / "channel" / IntVar(id) =>
      Recommendations[F].findRecommendationsOnChannel(Channel.Id(id), LocalDate.now()).map(_.toString).leftWiden[Throwable].flatMap(Ok(_))
    case GET -> Root / "recommendations" =>
      for {
        now <- currentDate
        result <- Recommendations[F]
          .findRecommendations(now)
          .flatMap(RecommendationTemplating[F].build)
          .leftWiden[Throwable]
          .flatMap(Ok(_, `Content-Type`(MediaType.text.html)))
      } yield result
    case POST -> Root / "recommendations" =>
      for {
        now <- currentDate
        result <- RecommendationSender[F]
          .sendRecommendations(now)
          .leftWiden[Throwable]
          .flatMap(_ => Ok("Sent"))
      } yield result
  }

  private def currentDate = Sync[F].syncThrowable(LocalDate.now(zone))
}
