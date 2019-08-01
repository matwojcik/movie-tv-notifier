package matwojcik.movies.recommendation

import java.time.LocalDate

import cats.effect.Sync
import cats.syntax.all._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import matwojcik.movies.config.MailConfig
import matwojcik.movies.mailing.Mails

trait RecommendationSender[F[_]] {
  def sendRecommendations(date: LocalDate): F[Unit]
}

object RecommendationSender {
  def apply[F[_]](implicit ev: RecommendationSender[F]): RecommendationSender[F] = ev

  def instance[F[_]: Sync: Recommendations: RecommendationTemplating: Mails](config: MailConfig): RecommendationSender[F] =
    new RecommendationSender[F] {
      private val logger = Slf4jLogger.getLogger[F]

      override def sendRecommendations(date: LocalDate): F[Unit] =
        for {
          recommendations <- Recommendations[F].findRecommendations(date)
          template        <- RecommendationTemplating[F].build(recommendations)
          _               <- Mails[F].send(config.to, s"Tv Recommendations for $date", template)
          _               <- logger.info(s"Sent recommendation email for $date")
        } yield ()

    }
}
