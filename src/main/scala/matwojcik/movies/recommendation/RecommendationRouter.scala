package matwojcik.movies.recommendation

import java.time.{LocalDate, LocalDateTime, ZoneId}

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.domain.Channel
import org.http4s.dsl.Http4sDsl
import org.http4s.headers._
import org.http4s.HttpRoutes
import org.http4s.MediaType
import scalatags.Text.all._

class RecommendationRouter[F[_]: Sync: Recommendations] extends Http4sDsl[F] {

  implicit val dateOrdering: Ordering[LocalDateTime] = Ordering.by(_.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli)

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "recommendations" / "channel" / IntVar(id) =>
      Recommendations[F].findRecommendationsOnChannel(Channel.Id(id), LocalDate.now()).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "recommendations" =>
      Recommendations[F]
        .findRecommendations(LocalDate.now())
        .map { recommendations =>
          val (topMovies, rest) = recommendations.sortBy(-_.movie.rating).splitAt(5)
          val recommendationsInOrderOfTime = rest.sortBy(_.date)

          html(
            head(
              meta(charset := "UTF-8"),
              link(rel := "stylesheet", href := "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"),
              tag("style")(
                """
                  |div.poster {
                  |      width: 170px;
                  |      float: left;
                  |    }
                  |    div.movie-content {
                  |      width: 500px;
                  |        float: left;
                  |    }
                  |
                  |    div.movie {
                  |      clear: left;
                  |      padding-top: 20px;
                  |    }
                  |    #movies {
                  |      clear: left;
                  |      padding-top: 50px;
                  |    }
                  |""".stripMargin)
            ),
            body(
              h1("Recommendations"),
              div(id:="top-movies", h2("Top movies"), topMovies.map(movieTemplate)),
              div(id:="movies", h2("Others"),recommendationsInOrderOfTime.map(movieTemplate))
            )
          ).toString()
        }
        .flatMap(Ok(_, `Content-Type`(MediaType.text.html)))
  }

  private def movieTemplate(recommendation: domain.Recommendation) = {
    div(`class` := "movie",
      div(`class` := "poster", img(src := recommendation.movie.poster.map(_.toString).getOrElse(""))),
      div(
        `class` := "movie-content",
        h4(
          recommendation.movie.url
            .map(url => a(href := url.toString, recommendation.movie.title))
            .getOrElse(recommendation.movie.title)
        ),
        h6(
          BigDecimal(recommendation.movie.rating).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
            + s" (${recommendation.movie.voteCount})"
        ),
        h6(recommendation.channel.name + " " + recommendation.date.toLocalTime.toString),
        p(recommendation.movie.plot.getOrElse("-"): String)
      )
    )
  }
}
