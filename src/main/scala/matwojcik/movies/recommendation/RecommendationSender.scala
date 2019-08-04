package matwojcik.movies.recommendation

import java.time.LocalDate

import au.id.tmm.bfect.effects.Sync._
import au.id.tmm.bfect.effects.Bracket
import au.id.tmm.bfect.effects.Sync
import matwojcik.movies.config.MailConfig
import matwojcik.movies.filmweb.FilmwebClient
import matwojcik.movies.mailing.Mails
import matwojcik.movies.recommendation.RecommendationSender.RecommendationSenderFailure
import matwojcik.movies.bfect.implicits._
import matwojcik.movies.util.Logger

trait RecommendationSender[F[+_, +_]] {
  def sendRecommendations(date: LocalDate): F[RecommendationSenderFailure, Unit]
}

object RecommendationSender {
  def apply[F[+_, +_]](implicit ev: RecommendationSender[F]): RecommendationSender[F] = ev

  def instance[F[+_, +_]: Sync: Bracket: Recommendations: RecommendationTemplating: Mails](config: MailConfig): RecommendationSender[F] =
    new RecommendationSender[F] {
      private val logger = Logger.getLogger[F, RecommendationSenderFailure](UnknownError)

      override def sendRecommendations(date: LocalDate): F[RecommendationSenderFailure, Unit] =
        for {
          recommendations <- Recommendations[F].findRecommendations(date).refineError(ClientError)
          template        <- RecommendationTemplating[F].build(recommendations)
          _               <- Mails[F].send(config.to, s"Tv Recommendations for $date", template).refineError(MailingError)
          _               <- logger.info(s"Sent recommendation email for $date")
        } yield ()

    }

  sealed trait RecommendationSenderFailure extends Throwable
  case class ClientError(cause: FilmwebClient.ClientError) extends RecommendationSenderFailure
  case class MailingError(cause: Mails.MailingError) extends RecommendationSenderFailure
  case class UnknownError(cause: Throwable) extends RecommendationSenderFailure

}
