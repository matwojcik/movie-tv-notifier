package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.Parallel
import cats.effect.Sync
import cats.syntax.all._
import cats.instances.all._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.domain.{Channel, TvSchedule}
import matwojcik.movies.recommendation.domain.Recommendation

trait Recommendations[F[_]] {
  def findRecommendationsOnChannel(channel: Channel.Id, date: LocalDate): F[List[Recommendation]]
}

object Recommendations {
  def apply[F[_]](implicit ev: Recommendations[F]): Recommendations[F] = ev

  def instance[F[_]: Sync: Filmweb, G[_]](implicit P: Parallel[F, G]) = new Recommendations[F] {
    override def findRecommendationsOnChannel(channel: Channel.Id, date: LocalDate): F[List[Recommendation]] = for {
      schedule <- Filmweb[F].findTvSchedule(channel, date)
      movies = schedule.collect{
        case schedule @ TvSchedule(Some(id), _, _, _) => id -> schedule
      }
      downloadedMovies <- movies.parTraverse{
        case (id, schedule) => Filmweb[F].findMovie(id).tupleRight(schedule)
      }
      interestingMovies = downloadedMovies.map{
        case (movie, schedule) => Recommendation(movie, channel, schedule.start)
      }.filter(schedule => schedule.movie.rating > 7.2 && schedule.movie.voteCount > 2000)
    } yield interestingMovies
  }
}