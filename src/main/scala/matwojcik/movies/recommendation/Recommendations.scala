package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.Parallel
import cats.effect.Sync
import cats.instances.all._
import cats.syntax.all._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.TvSchedule
import matwojcik.movies.recommendation.domain.Recommendation

trait Recommendations[F[_]] {
  def findRecommendationsOnChannel(channelId: Channel.Id, date: LocalDate): F[List[Recommendation]]
  def findRecommendations(date: LocalDate): F[List[Recommendation]]
}

object Recommendations {
  def apply[F[_]](implicit ev: Recommendations[F]): Recommendations[F] = ev

  def instance[F[_]: Sync: Filmweb, G[_]](implicit P: Parallel[F, G]) = new Recommendations[F] {

    override def findRecommendationsOnChannel(channelId: Channel.Id, date: LocalDate): F[List[Recommendation]] =
      for {
        channels <- Filmweb[F].findAllChannels()
        channel = channels.find(_.id == channelId)
        recommendations <- channel.map(findRecommendationsOnChannel(_, date)).getOrElse(List.empty[Recommendation].pure[F])
      } yield recommendations

    private def findRecommendationsOnChannel(channel: Channel, date: LocalDate): F[List[Recommendation]] =
      for {
        schedule <- Filmweb[F].findTvSchedule(channel.id, date)
        movies = filterMovies(schedule)
        downloadedMovies <- movies.parTraverse {
          case (id, schedule) => Filmweb[F].findMovie(id).tupleRight(schedule)
        }
        interestingMovies = downloadedMovies.map {
          case (movie, schedule) => Recommendation(movie, channel, schedule.start)
        }.filter(schedule => schedule.movie.rating > 7.2 && schedule.movie.voteCount > 5000)
      } yield interestingMovies

    private def filterMovies(schedule: List[TvSchedule]) =
      schedule.filter(notTvShows).collect {
        case schedule @ TvSchedule(Some(id), _, _, _) => id -> schedule
      }

    private def notTvShows(program: TvSchedule) =
      List("serial", "magazyn", "program").forall(!program.description.contains(_))

    override def findRecommendations(date: LocalDate): F[List[Recommendation]] =
      for {
        channels        <- Filmweb[F].findAllChannels()
        recommendations <- channels.take(100).traverse(findRecommendationsOnChannel(_, date))
      } yield recommendations.flatten

  }
}
