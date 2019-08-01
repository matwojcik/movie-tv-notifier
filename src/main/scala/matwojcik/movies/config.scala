package matwojcik.movies

import matwojcik.movies.filmweb.FilmwebConfig

object config {
  case class Config(smtp: SMTPConfig, filmweb: FilmwebConfig)
  case class SMTPConfig(address: String, port: Int, username: String, password: String)
}
