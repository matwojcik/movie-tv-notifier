package matwojcik.movies.recommendation

import java.time.LocalDateTime

import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.Movie

object domain {
  case class Recommendation(movie: Movie, channel: Channel, date: LocalDateTime)
}
