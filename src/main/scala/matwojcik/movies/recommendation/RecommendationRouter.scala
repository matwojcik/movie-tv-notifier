package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.domain.Channel
import org.http4s.HttpRoutes
import org.http4s.MediaType
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._

class RecommendationRouter[F[_]: Sync: Recommendations: RecommendationTemplating: RecommendationSender] extends Http4sDsl[F] {

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "recommendations" / "channel" / IntVar(id) =>
      Recommendations[F].findRecommendationsOnChannel(Channel.Id(id), LocalDate.now()).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "recommendations" =>
      Recommendations[F]
        .findRecommendations(LocalDate.now())
        .flatMap(RecommendationTemplating[F].build)
        .flatMap(Ok(_, `Content-Type`(MediaType.text.html)))
    case GET -> Root / "recommendations" / "send" =>
      RecommendationSender[F]
        .sendRecommendations(LocalDate.now())
        .flatMap(_ => Ok("Sent"))
  }
}
