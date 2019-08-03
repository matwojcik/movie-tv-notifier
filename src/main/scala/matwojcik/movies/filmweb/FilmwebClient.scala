package matwojcik.movies.filmweb

import au.id.tmm.bfect.BifunctorMonad
import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects.Die._
import au.id.tmm.bfect.effects._
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp._
import io.circe.Json
import matwojcik.movies.filmweb.FilmwebClient.ClientError.ThrowableToClientError
import matwojcik.movies.filmweb.FilmwebClient.Decoder.DecodingFailure
import matwojcik.movies.filmweb.FilmwebClient.ClientError
import matwojcik.movies.filmweb.FilmwebClient.Decoder
import matwojcik.movies.filmweb.FilmwebClient.UnexpectedError
import matwojcik.movies.util.BifunctorImplicits._
import matwojcik.movies.util.Logger
import org.apache.commons.codec.digest.DigestUtils

class FilmwebClient[F[+_, +_]: Sync: Bracket](config: FilmwebConfig, sttpBackend: SttpBackend[F[Throwable, ?], Nothing]) {
  private val logger = Logger.getLogger[F, ClientError](ThrowableToClientError)

  def executeMethod[A: Decoder](methodName: String, params: List[String]): F[ClientError, A] = {
    val methodWithParams = methodName + " [" + params.mkString(",") + "]\\n" // :o
    val signature = FilmwebClient.ApiVersion + "," + DigestUtils.md5Hex(methodWithParams + FilmwebClient.AppId + config.apiKey)

    val url =
      uri"${config.baseUrl}?methods=$methodWithParams&signature=$signature&version=${FilmwebClient.ApiVersion}&appId=${FilmwebClient.AppId}"

    logger.debug(s"Calling $url") *>
      sttpBackend.send(sttp.get(url)).refineError(ThrowableToClientError).flatMap {
        _.body match {
          case Left(failure) =>
            failWithReason(s"Failed call to Filmweb due to $failure")
          case Right(response) =>
            logger.debug(s"Got response: $response") *>
              parseResponse(response)
        }
      }
  }

  private def parseResponse[A: Decoder](response: String): F[ClientError, A] = {
    val correctResponsePattern = """^ok\n(\[.*\])[^\]]+$""".r // :o
    response match {
      case correctResponsePattern(result) =>
        import io.circe.parser._
        parse(result) match {
          case Left(failure) =>
            failWithReason(s"Failed parsing json due to $failure")
          case Right(json) =>
            json.asArray.map { v =>
              BifunctorMonad[F].fromEither(Decoder[A].decode(v))
            }.getOrElse(failWithReason[A](s"Failed parsing $json to array"))
        }
      case _ =>
        failWithReason(s"Couldn't find array in response: $response")
    }

  }

  private def failWithReason[A](reason: String): F[ClientError, A] =
    Sync[F].raiseError[ClientError](new UnexpectedError(new RuntimeException(reason)))
}

object FilmwebClient {
  val AppId = "android"
  val ApiVersion = "1.0"

  sealed trait ClientError extends Throwable

  object ClientError {
    val ThrowableToClientError: Throwable => ClientError = new UnexpectedError(_)
  }

  class UnexpectedError(cause: Throwable) extends RuntimeException(cause) with ClientError

  trait Decoder[A] {
    def decode(v: Vector[Json]): Either[DecodingFailure, A]
  }

  object Decoder {
    def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev
    class DecodingFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) with ClientError
  }

}
