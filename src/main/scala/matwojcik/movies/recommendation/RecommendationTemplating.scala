package matwojcik.movies.recommendation

import java.time.LocalDateTime
import java.time.ZoneId

import au.id.tmm.bfect.BifunctorMonad
import matwojcik.movies.filmweb.domain.Movie
import matwojcik.movies.recommendation.domain.Recommendation
import scalatags.Text.all._

trait RecommendationTemplating[F[+_,+_]] {
  def build(recommendations: List[Recommendation]): F[Nothing, String]
}

object RecommendationTemplating {
  def apply[F[+_,+_]](implicit ev: RecommendationTemplating[F]): RecommendationTemplating[F] = ev

  def instance[F[+_,+_]: BifunctorMonad]: RecommendationTemplating[F] = new RecommendationTemplating[F] {

    implicit val dateOrdering: Ordering[LocalDateTime] = Ordering.by(_.atZone(ZoneId.systemDefault()).toInstant.toEpochMilli)

    override def build(recommendations: List[Recommendation]): F[Nothing, String] = BifunctorMonad[F].pure {
      val (topMovies, rest) = recommendations.sortBy(-_.movie.rating).splitAt(5)
      val recommendationsInOrderOfTime = rest.sortBy(_.date)

      html(
        head(
          meta(charset := "UTF-8"),
          tag("style")("""
                         |  body {
                         |    font-family:Helvetica, sans-serif
                         |  }
                         |  h3 {
                         |    margin-top: 0
                         |  }
                         |  a {
                         |    text-decoration: none;
                         |    color: #0099ff
                         |  }
                         |  a:hover{
                         |    color: #008ae6;
                         |  }
                         |  div.poster {
                         |    width: 170px;
                         |    float: left;
                         |  }
                         |  div.movie-content {
                         |    width: 500px;
                         |    float: left;
                         |  }
                         |
                         |  div.movie {
                         |    clear: left;
                         |    padding-top: 20px;
                         |  }
                         |  #movies {
                         |    clear: left;
                         |    padding-top: 50px;
                         |  }
                         |""".stripMargin)
        ),
        body(
          h1("Recommendations"),
          div(id := "top-movies", h2("Top movies"), topMovies.map(movieTemplate)),
          div(id := "movies", h2("Others"), recommendationsInOrderOfTime.map(movieTemplate))
        )
      ).toString()
    }

    private def movieTemplate(recommendation: domain.Recommendation) = recommendation match {
      case Recommendation(movie, channel, date) =>
        div(
          `class` := "movie",
          div(`class` := "poster", img(src := movie.poster.map(_.toString).getOrElse(""))),
          div(
            `class` := "movie-content",
            h3(
              movie.url
                .map(url => a(href := url.toString, movie.title))
                .getOrElse(movie.title)
            ),
            h4(
              roundedRating(movie) + s" (${movie.voteCount})"
            ),
            h4(channel.name + " " + date.toLocalTime.toString),
            p(movie.plot.getOrElse("-"): String)
          )
        )
    }

    private def roundedRating(movie: Movie) =
      BigDecimal(movie.rating).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }
}
