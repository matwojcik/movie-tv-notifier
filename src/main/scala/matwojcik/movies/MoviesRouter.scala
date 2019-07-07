package matwojcik.movies

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class MoviesRouter[F[_]: Sync] extends Http4sDsl[F] {

  val service: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "hello" =>
      Ok("Hello world")
  }

}
