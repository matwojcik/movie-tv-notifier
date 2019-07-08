package matwojcik.movies

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.Movie
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class MoviesRouter[F[_]: Sync: Filmweb] extends Http4sDsl[F] {

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "movie" / IntVar(id) =>
      Filmweb[F].findMovie(Movie.Id(id)).map(_.toString).flatMap(Ok(_))
    case GET -> Root / "tv" / "channel" / IntVar(id) =>
      Filmweb[F].findTvSchedule(Channel.Id(id)).map(_.mkString(":::")).flatMap(Ok(_))
  }

}
