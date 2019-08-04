package matwojcik.movies

import java.time.LocalDate

import au.id.tmm.bfect.effects.{Bracket, Sync}
import au.id.tmm.bfect.catsinterop._
import cats.syntax.all._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.domain.{Channel, Movie, User}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class MoviesRouter[F[+_,+_]: Sync: Bracket: Filmweb] extends Http4sDsl[F[Throwable,?]] {

  val service: HttpRoutes[F[Throwable,?]] = HttpRoutes.of[F[Throwable,?]] {
    case GET -> Root / "movie" / IntVar(id) =>
      Filmweb[F].findMovie(Movie.Id(id)).map(_.toString).leftWiden[Throwable].flatMap(Ok(_))
    case GET -> Root / "tv" / "channels" =>
      Filmweb[F].findAllChannels().map(_.toString()).leftWiden[Throwable].flatMap(Ok(_))
    case GET -> Root / "tv" / "channels" / IntVar(id) =>
      Filmweb[F].findTvSchedule(Channel.Id(id), LocalDate.now()).map(_.mkString(":::")).leftWiden[Throwable].flatMap(Ok(_))
  }

}
