package matwojcik.movies.filmweb

import cats.effect.Sync
import cats.instances.all._
import cats.syntax.all._
import io.circe.Json
import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.Movie
import matwojcik.movies.filmweb.domain.TvSchedule

import scala.util.Success
import scala.util.Try

trait Filmweb[F[_]] {
  def findMovie(id: Movie.Id): F[Movie]
  def findTvSchedule(id: Channel.Id): F[List[TvSchedule]]
}

object Filmweb {
  def apply[F[_]](implicit ev: Filmweb[F]): Filmweb[F] = ev

  def instance[F[_]: Sync](client: FilmwebClient[F]): Filmweb[F] = new Filmweb[F] {
    override def findMovie(id: Movie.Id): F[Movie] =
      client.runMethod("getFilmInfoFull", List(id.value.toString))(parseMovie)

    override def findTvSchedule(id: Channel.Id): F[List[TvSchedule]] =
      client.runMethod("getTvSchedule", List(id.value.toString, "2019-07-10"))(parseSchedules)

    private def parseMovie(v: Vector[Json]): F[Movie] = {
      val title = Try(v(0)).map(_.as[String]).flatMap(_.toTry)
      val rating = Try(v(2)).map(_.as[Double]).flatMap(_.toTry)
      (title, rating).bisequence match {
        case Success(values) =>
          (Movie.apply _).tupled(values).pure[F]
        case _ => Sync[F].raiseError(new RuntimeException("Incorrect type"))
      }
    }

    private def parseSchedules(v: Vector[Json]): F[List[TvSchedule]] =
      v.traverse {
        _.asArray.map(parseSchedule).getOrElse(Sync[F].raiseError(new RuntimeException("Incorrect type")))
      }.map(_.toList)

    private def parseSchedule(v: Vector[Json]): F[TvSchedule] = {
      val title = Try(v(1)).map(_.as[String]).flatMap(_.toTry)
      val description = Try(v(2)).map(_.as[String]).flatMap(_.toTry)
      val start = Try(v(3)).map(_.as[String]).flatMap(_.toTry)
      val id = Try(v(5)).map(_.as[Option[Int]].map(_.map(Movie.Id))).flatMap(_.toTry)

      (id, title, description, start).mapN {
        case values => (TvSchedule.apply _).tupled(values).pure[F]
      }.getOrElse(Sync[F].raiseError(new RuntimeException("Incorrect type")))

    }
  }
}
