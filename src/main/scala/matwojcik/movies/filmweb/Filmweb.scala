package matwojcik.movies.filmweb

import java.time.{LocalDate, LocalDateTime, LocalTime, Year}
import java.time.format.DateTimeFormatter

import cats.effect.Sync
import cats.instances.all._
import cats.syntax.all._
import io.circe.Json
import matwojcik.movies.filmweb.domain.{Channel, Movie, TvSchedule, User}

import scala.util.Success
import scala.util.Try

trait Filmweb[F[_]] {
  def findMovie(id: Movie.Id): F[Movie]
  def findTvSchedule(id: Channel.Id, date: LocalDate): F[List[TvSchedule]]
  def findMoviesWatchList(id: User.Id): F[String]
  def findAllChannels(): F[List[Channel]]
}

object Filmweb {
  def apply[F[_]](implicit ev: Filmweb[F]): Filmweb[F] = ev

  def instance[F[_]: Sync](client: FilmwebClient[F]): Filmweb[F] = new Filmweb[F] {
    override def findMovie(id: Movie.Id): F[Movie] =
      client.runMethod("getFilmInfoFull", List(id.value.toString))(parseMovie)

    override def findTvSchedule(id: Channel.Id, localDate: LocalDate): F[List[TvSchedule]] =
      client.runMethod("getTvSchedule", List(id.value.toString, localDate.format(DateTimeFormatter.ISO_DATE)))(parseSchedules(localDate))

    override def findMoviesWatchList(id: User.Id): F[String] =
      client.runMethod("getUserFilmsWantToSee", List(id.value.toString, "1", "1000"))(_.mkString(":::").pure[F])

    override def findAllChannels(): F[List[Channel]] =
      client.runMethod("getAllChannels", List("\"\""))(parseChannels)

    private def parseMovie(v: Vector[Json]): F[Movie] = {
      val title = Try(v(0)).map(_.as[String]).flatMap(_.toTry)
      val rating = Try(v(2)).map(_.as[Double]).flatMap(_.toTry)
      val voteCount = Try(v(3)).map(_.as[Int]).flatMap(_.toTry)
      val plot = Try(v(19)).map(_.as[Option[String]]).flatMap(_.toTry)
      val year = Try(v(5)).map(_.as[Int].map(Year.of)).flatMap(_.toTry)
      (title, year, plot, rating, voteCount).mapN {
        case values =>
          (Movie.apply _).tupled(values).pure[F]
      }.getOrElse(Sync[F].raiseError(new RuntimeException(s"Incorrect type: $v")))
    }

    private def parseSchedules(date: LocalDate)(v: Vector[Json]): F[List[TvSchedule]] =
      v.traverse {
        _.asArray.map(parseSchedule(date)).getOrElse(Sync[F].raiseError(new RuntimeException("Incorrect type")))
      }.map(_.toList)

    private def parseSchedule(date: LocalDate)(v: Vector[Json]): F[TvSchedule] = {
      val title = Try(v(1)).map(_.as[String]).flatMap(_.toTry)
      val description = Try(v(2)).map(_.as[String]).flatMap(_.toTry)
      val start = Try(v(3)).map(_.as[String].map(LocalTime.parse(_, DateTimeFormatter.ofPattern("k:mm"))).map(LocalDateTime.of(date, _))).flatMap(_.toTry)
      val id = Try(v(5)).map(_.as[Option[Int]].map(_.map(Movie.Id))).flatMap(_.toTry)

      (id, title, description, start).mapN {
        case values => (TvSchedule.apply _).tupled(values).pure[F]
      }.getOrElse(Sync[F].raiseError(new RuntimeException(s"Incorrect type: $v")))

    }


    private def parseChannels(v: Vector[Json]): F[List[Channel]] =
      v.tail.traverse { v3 =>
        v3.asArray.map(parseChannel).getOrElse(Sync[F].raiseError(new RuntimeException(s"Incorrect type: $v3")))
      }.map(_.toList)

    private def parseChannel(v: Vector[Json]): F[Channel] = {
      val id = Try(v(0)).map(_.as[Int].map(Channel.Id)).flatMap(_.toTry)
      val name = Try(v(1)).map(_.as[String]).flatMap(_.toTry)
      (id, name).mapN {
        case values =>
          (Channel.apply _).tupled(values).pure[F]
      }.getOrElse(Sync[F].raiseError(new RuntimeException(s"Incorrect type: $v")))
    }

  }
}
