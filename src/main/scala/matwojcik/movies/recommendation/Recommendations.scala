package matwojcik.movies.recommendation

import java.time.LocalDate

import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects.Sync._
import au.id.tmm.bfect.effects.Bracket
import au.id.tmm.bfect.effects.Sync
import cats.instances.all._
import cats.syntax.all._
import matwojcik.movies.bfect.BifunctorParallel
import matwojcik.movies.bfect.implicits._
import matwojcik.movies.filmweb.Filmweb
import matwojcik.movies.filmweb.FilmwebClient.ClientError
import matwojcik.movies.filmweb.domain.Channel
import matwojcik.movies.filmweb.domain.TvSchedule
import matwojcik.movies.recommendation.domain.Recommendation
import matwojcik.movies.util.Logger

trait Recommendations[F[+_, +_]] {
  def findRecommendationsOnChannel(channelId: Channel.Id, date: LocalDate): F[ClientError, List[Recommendation]]
  def findRecommendations(date: LocalDate): F[ClientError, List[Recommendation]]
}

object Recommendations {
  def apply[F[+_, +_]](implicit ev: Recommendations[F]): Recommendations[F] = ev

  def instance[F[+_, +_]: Sync: Bracket: Filmweb, G[+_, +_]](implicit P: BifunctorParallel[F, G]): Recommendations[F] =
    new Recommendations[F] {

      private val logger = Logger.getLogger[F, ClientError](ClientError.apply)

      override def findRecommendationsOnChannel(channelId: Channel.Id, date: LocalDate): F[ClientError, List[Recommendation]] =
        for {
          channels <- Filmweb[F].findAllChannels()
          channel = channels.find(_.id == channelId)
          recommendations <- channel.map(findRecommendationsOnChannel(_, date)).getOrElse(Sync[F].pure(List.empty[Recommendation]))
        } yield recommendations

      private def findRecommendationsOnChannel(channel: Channel, date: LocalDate): F[ClientError, List[Recommendation]] =
        for {
          schedule <- Filmweb[F].findTvSchedule(channel.id, date)
          movies = filterMovies(schedule)
          downloadedMovies <- movies.parTraverse {
                               case (id, schedule) => Filmweb[F].findMovie(id).tupleRight(schedule)
                             }
          interestingMovies = downloadedMovies
            .map {
              case (movie, schedule) => Recommendation(movie, channel, schedule.start)
            }
            .filter(schedule => schedule.movie.rating > 7.2 && schedule.movie.voteCount > 10000)
        } yield interestingMovies

      private def filterMovies(schedule: List[TvSchedule]) =
        schedule.filter(notTvShows).collect {
          case schedule @ TvSchedule(Some(id), _, _, _) => id -> schedule
        }

      private def notTvShows(program: TvSchedule) =
        List("serial", "magazyn", "program").forall(w => !program.description.exists(_.contains(w)))

      override def findRecommendations(date: LocalDate): F[ClientError, List[Recommendation]] =
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
