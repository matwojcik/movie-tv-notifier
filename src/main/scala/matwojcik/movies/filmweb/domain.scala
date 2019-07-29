package matwojcik.movies.filmweb

import java.time.{LocalDateTime, Year}

object domain {
  case class Movie(title: String, year: Year, plot: Option[String], rating: Double, voteCount: Int)

  object Movie {
    case class Id(value: Int) extends AnyVal
  }
  object Channel {
    case class Id(value: Int) extends AnyVal
  }

  case class TvSchedule(id: Option[Movie.Id], title: String, description: String, start: LocalDateTime)

  object User {
    case class Id(value: Int) extends AnyVal
  }
}
