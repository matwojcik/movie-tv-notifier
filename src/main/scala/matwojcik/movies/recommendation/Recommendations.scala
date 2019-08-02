package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.Parallel
import cats.effect.Sync
import cats.instances.all._
import cats.syntax.all._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
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

    private val logger = Slf4jLogger.getLogger[F]

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
        }.filter(schedule => schedule.movie.rating > 7.2 && schedule.movie.voteCount > 10000)
      } yield interestingMovies

    private def filterMovies(schedule: List[TvSchedule]) =
      schedule.filter(notTvShows).collect {
        case schedule @ TvSchedule(Some(id), _, _, _) => id -> schedule
      }

    private def notTvShows(program: TvSchedule) =
      List("serial", "magazyn", "program").forall(w => !program.description.exists(_.contains(w)))

    override def findRecommendations(date: LocalDate): F[List[Recommendation]] =
      for {
        channels        <- findChannelsOfUser
        recommendations <- channels.parTraverse(findRecommendationsOnChannel(_, date)).map(_.flatten)
        _               <- logger.debug(s"Interesting movies for $date : $recommendations")
      } yield recommendations

    private def findChannelsOfUser =
      Filmweb[F]
        .findAllChannels()
        .map(
          _.filter(
            interestingChannels contains _.id.value
          )
        )
  }

  val interestingChannels = List(1, 2, 3, 4, 5, 8, 11, 110, 22, 98, 9, 10, 156, 32, 157, 31, 33, 53, 60, 40, 25, 109, 124, 420, 75, 74, 117,
    418, 71, 29, 28, 27, 78, 361, 41, 38, 103, 64, 36, 108, 47)
}
