package matwojcik.movies.filmweb

object domain {
  case class Movie(title: String, rating: Double)

  object Movie {
    case class Id(value: Int) extends AnyVal
  }
  object Channel {
    case class Id(value: Int) extends AnyVal
  }

  case class TvSchedule(id: Option[Movie.Id], title: String, description: String, start: String)
}
