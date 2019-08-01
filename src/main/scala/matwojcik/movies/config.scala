package matwojcik.movies

import matwojcik.movies.filmweb.FilmwebConfig

object config {
  case class Config(http: HttpConfig, mail: MailConfig, filmweb: FilmwebConfig)
  case class HttpConfig(host: String, port: Int)
  case class MailConfig(smtp: SMTPConfig, from: String, to: String)
  case class SMTPConfig(address: String, port: Int, username: String, password: String)
}
