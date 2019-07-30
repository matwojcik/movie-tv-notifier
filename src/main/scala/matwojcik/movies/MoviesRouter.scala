package matwojcik.movies

import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.domain.{Channel, Movie, User}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class MoviesRouter[F[_]: Sync: Filmweb] extends Http4sDsl[F] {

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "movie" / IntVar(id) =>
      Filmweb[F].findMovie(Movie.Id(id)).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "tv" / "channels" =>
      Filmweb[F].findAllChannels().map(_.toString()).flatMap(Ok(_))
    case GET -> Root / "tv" / "channels" / IntVar(id) =>
      Filmweb[F].findTvSchedule(Channel.Id(id), LocalDate.now()).map(_.mkString(":::")).flatMap(Ok(_))
    case GET -> Root / "user" / IntVar(id) / "watchlist" =>
      Filmweb[F].findMoviesWatchList(User.Id(id)).flatMap(Ok(_))
  }

}
