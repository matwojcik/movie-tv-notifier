package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.domain.Channel
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class RecommendationRouter[F[_]: Sync: Recommendations] extends Http4sDsl[F] {

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "recommendations" / "channel" / IntVar(id) =>
      Recommendations[F].findRecommendationsOnChannel(Channel.Id(id), LocalDate.now()).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "recommendations" =>
      Recommendations[F].findRecommendations(LocalDate.now()).map(_.toString).flatMap(Ok(_))
  }

}
