package matwojcik.movies.mailing

import au.id.tmm.bfect.effects.{Bracket, Sync}
import au.id.tmm.bfect.catsinterop._
import cats.syntax.all._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import matwojcik.movies.config.MailConfig
import matwojcik.movies.mailing.Mails.MailingError
import matwojcik.movies.util.Logger
import org.simplejavamail.email.EmailBuilder
import org.simplejavamail.mailer.MailerBuilder
import org.simplejavamail.mailer.config.TransportStrategy

trait Mails[F[+_,+_]] {
  def send(to: String, subject: String, content: String): F[MailingError, Unit]
}

object Mails {
  def apply[F[+_,+_]](implicit ev: Mails[F]): Mails[F] = ev

  def instance[F[+_,+_]: Sync: Bracket](config: MailConfig): Mails[F] = new Mails[F] {
    private val logger = Logger.getLogger[F,MailingError](MailingError)

    private val mailer = MailerBuilder
      .withSMTPServer(config.smtp.address, config.smtp.port, config.smtp.username, config.smtp.password)
      .withTransportStrategy(TransportStrategy.SMTP_TLS)
      .withSessionTimeout(10 * 1000)
      .clearEmailAddressCriteria() // turns off email validation
      .buildMailer()

    override def send(to: String, subject: String, content: String): F[MailingError, Unit] = {
      val email = EmailBuilder
        .startingBlank()
        .to(to)
        .from(config.from)
        .withSubject(subject)
        .withHTMLText(content)
        .buildEmail();

      Sync[F].syncCatch(mailer.sendMail(email)){
        case t => MailingError(t)
      } *> logger.info(s"Sent e-mail to $to")
    }
  }

  case class MailingError(cause: Throwable) extends Throwable


}
