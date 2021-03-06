package matwojcik.movies.filmweb

import java.net.URL
import java.time.{LocalDateTime, Year}

object domain {
  // TODO add english title
  case class Movie(title: String, year: Year, plot: Option[String], rating: Double, voteCount: Int, url: Option[URL], poster: Option[URL])

  object Movie {
    case class Id(value: Int) extends AnyVal
  }

  case class Channel(id: Channel.Id, name: String)

  object Channel {
    case class Id(value: Int) extends AnyVal
  }

  case class TvSchedule(id: Option[Movie.Id], title: String, description: Option[String], start: LocalDateTime)

  object User {
    case class Id(value: Int) extends AnyVal
  }
}
