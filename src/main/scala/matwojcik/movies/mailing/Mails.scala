package matwojcik.movies.mailing

import cats.effect.Sync
import cats.syntax.all._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import matwojcik.movies.config.MailConfig
import org.simplejavamail.email.EmailBuilder
import org.simplejavamail.mailer.MailerBuilder
import org.simplejavamail.mailer.config.TransportStrategy

trait Mails[F[_]] {
  def send(to: String, subject: String, content: String): F[Unit]
}

object Mails {
  def apply[F[_]](implicit ev: Mails[F]): Mails[F] = ev

  def instance[F[_]: Sync](config: MailConfig): Mails[F] = new Mails[F] {
    private val logger = Slf4jLogger.getLogger[F]

    private val mailer = MailerBuilder
      .withSMTPServer(config.smtp.address, config.smtp.port, config.smtp.username, config.smtp.password)
      .withTransportStrategy(TransportStrategy.SMTP_TLS)
      .withSessionTimeout(10 * 1000)
      .clearEmailAddressCriteria() // turns off email validation
      .buildMailer()

    override def send(to: String, subject: String, content: String): F[Unit] = {
      val email = EmailBuilder
        .startingBlank()
        .to(to)
        .from(config.from)
        .withSubject(subject)
        .withHTMLText(content)
        .buildEmail();

      Sync[F].delay(mailer.sendMail(email)) *> logger.info(s"Sent e-mail to $to")
    }
  }
}
