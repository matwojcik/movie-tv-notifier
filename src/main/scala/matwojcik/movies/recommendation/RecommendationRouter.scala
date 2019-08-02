package matwojcik.movies.recommendation

import java.time.LocalDate
import java.time.ZoneId

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.domain.Channel
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._

class RecommendationRouter[F[_]: Sync: Recommendations: RecommendationTemplating: RecommendationSender] extends Http4sDsl[F] {

  val zone = ZoneId.of("Europe/Warsaw")

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "recommendations" / "channel" / IntVar(id) =>
      Recommendations[F].findRecommendationsOnChannel(Channel.Id(id), LocalDate.now()).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "recommendations" =>
      for {
        now <- currentDate
        result <- Recommendations[F]
          .findRecommendations(now)
          .flatMap(RecommendationTemplating[F].build)
          .flatMap(Ok(_, `Content-Type`(MediaType.text.html)))
      } yield result
    case POST -> Root / "recommendations" =>
      for {
        now <- currentDate
        result <- RecommendationSender[F]
          .sendRecommendations(now)
          .flatMap(_ => Ok("Sent"))
      } yield result
  }

  private def currentDate = Sync[F].delay(LocalDate.now(zone))
}
