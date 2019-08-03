package matwojcik.movies.filmweb

import java.net.URL
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.Year
import java.time.format.DateTimeFormatter

import cats.instances.all._
import cats.syntax.all._
import io.circe.Decoder
import io.circe.Json
import matwojcik.movies.filmweb.FilmwebClient.ClientError
import matwojcik.movies.filmweb.FilmwebClient.Decoder.DecodingFailure
import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.Movie
import matwojcik.movies.filmweb.domain.TvSchedule

import scala.util.Try

trait Filmweb[F[+_, +_]] {
  def findMovie(id: Movie.Id): F[ClientError, Movie]
  def findTvSchedule(id: Channel.Id, date: LocalDate): F[ClientError, List[TvSchedule]]
  def findAllChannels(): F[ClientError, List[Channel]]
}

object Filmweb {
  def apply[F[+_, +_]](implicit ev: Filmweb[F]): Filmweb[F] = ev

  def instance[F[+_, +_]](client: FilmwebClient[F]): Filmweb[F] = new Filmweb[F] {

    override def findMovie(id: Movie.Id): F[ClientError, Movie] =
      client.executeMethod[Movie]("getFilmInfoFull", List(id.value.toString))

    override def findTvSchedule(id: Channel.Id, localDate: LocalDate): F[ClientError, List[TvSchedule]] = {
      implicit val decoder: FilmwebClient.Decoder[List[TvSchedule]] = schedulesDecoder(localDate)
      client.executeMethod[List[TvSchedule]]("getTvSchedule", List(id.value.toString, localDate.format(DateTimeFormatter.ISO_DATE)))
    }

    override def findAllChannels(): F[ClientError, List[Channel]] =
      client.executeMethod[List[Channel]]("getAllChannels", List("\"\""))

    implicit val movieDecoder: FilmwebClient.Decoder[Movie] = (v: Vector[Json]) => {
      val title = parse[String](v)(0)
      val rating = parse[Double](v)(2)
      val voteCount = parse[Int](v)(3)
      val plot = parse[Option[String]](v)(19)
      val year = parse[Int](v)(5).map(Year.of)
      val url = parse[Option[String]](v)(8).map(_.map(_.replace("/discussion", "")).map(new URL(_)))
      val poster = parse[Option[String]](v)(11).map(_.map("http://1.fwcdn.pl/po" +_).map(new URL(_)))
      (title, year, plot, rating, voteCount, url, poster).mapN {
        case values =>
          (Movie.apply _).tupled(values)
      }.leftMap(decodingFailure("movie", v))
    }

    implicit def schedulesDecoder(date: LocalDate): FilmwebClient.Decoder[List[TvSchedule]] =
      (v: Vector[Json]) =>
        v.traverse {
          _.asArray
            .map(parseSchedule(date))
            .getOrElse(Left(decodingFailure("schedules", v)(new RuntimeException("Schedules not an array"))))
        }.map(_.toList)

    private def parseSchedule(date: LocalDate)(v: Vector[Json]): Either[DecodingFailure, TvSchedule] = {
      val title = parse[String](v)(1)
      val description = parse[Option[String]](v)(2)
      val start = parse[String](v)(3).map(LocalTime.parse(_, DateTimeFormatter.ofPattern("k:mm"))).map { time =>
        if (time.getHour > 5)
          LocalDateTime.of(date, time)
        else
          LocalDateTime.of(date.plusDays(1), time)
      }
      val id = parse[Option[Int]](v)(5).map(_.map(Movie.Id))

      (id, title, description, start).mapN {
        case values => (TvSchedule.apply _).tupled(values)
      }.leftMap(decodingFailure("schedule", v))

    }

    implicit val channelsDecoder: FilmwebClient.Decoder[List[Channel]] = (v: Vector[Json]) =>
      v match {
        case head +: tail =>
          tail.traverse { v3 =>
            v3.asArray.map(parseChannel).getOrElse(Left(decodingFailure("channels", v)(new RuntimeException("Channels not an array"))))
          }.map(_.toList)
        case _ =>
          Left(decodingFailure("channels", v)(new RuntimeException("Channels list didn't have enough elements")))
    }

    private def parseChannel(v: Vector[Json]): Either[DecodingFailure, Channel] = {
      val id = parse[Int](v)(0).map(Channel.Id)
      val name = parse[String](v)(1)
      (id, name).mapN {
        case values =>
          (Channel.apply _).tupled(values)
      }.leftMap(decodingFailure("channel", v))
    }

    private def parse[A: Decoder](v: Vector[Json])(index: Int) =
      Try(v(index)).map(_.as[A]).toEither.flatten

    private def decodingFailure[A](name: String, v: Vector[Json])(cause: Throwable) =
      new DecodingFailure(s"Error during parsing $name from $v", cause)
  }
}
