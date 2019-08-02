package matwojcik.movies.filmweb

import cats.effect.Sync
import cats.syntax.all._
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp._
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.circe.Json
import matwojcik.movies.filmweb.FilmwebClient.Decoder
import matwojcik.movies.filmweb.FilmwebClient.Decoder.DecodingFailure
import org.apache.commons.codec.digest.DigestUtils

class FilmwebClient[F[_]: Sync](config: FilmwebConfig, sttpBackend: SttpBackend[F, Nothing]) {

  private val logger = Slf4jLogger.getLogger[F]

  def executeMethod[A: Decoder](methodName: String, params: List[String]): F[A] = {
    val methodWithParams = methodName + " [" + params.mkString(",") + "]\\n" // :o
    val signature = FilmwebClient.ApiVersion + "," + DigestUtils.md5Hex(methodWithParams + FilmwebClient.AppId + config.apiKey)

    val url =
      uri"${config.baseUrl}?methods=$methodWithParams&signature=$signature&version=${FilmwebClient.ApiVersion}&appId=${FilmwebClient.AppId}"

    logger.debug(s"Calling $url") *>
      sttpBackend.send(sttp.get(url)).flatMap {
        _.body match {
          case Left(failure) =>
            failWithReason(s"Failed call to Filmweb due to $failure")
          case Right(response) =>
            logger.debug(s"Got response: $response") *>
              parseResponse(response)
        }
      }
  }

  private def parseResponse[A: Decoder](response: String): F[A] = {
    val correectResponsePattern = """^ok\n(\[.*\])[^\]]+$""".r // :o
    response match {
      case correectResponsePattern(result) =>
        import io.circe.parser._
        parse(result) match {
          case Left(failure) =>
            failWithReason(s"Failed parsing json due to $failure")
          case Right(json) =>
            json.asArray.map {
              Decoder[A].decode(_).fold(sttpBackend.responseMonad.error[A], _.pure[F])
            }.getOrElse(failWithReason(s"Failed parsing $json to array"))
        }
      case _ =>
        failWithReason(s"Couldn't find array in response: $response")
    }

  }

  private def failWithReason[A](reason: String) =
    sttpBackend.responseMonad.error[A](new RuntimeException(reason))
}

object FilmwebClient {
  val AppId = "android"
  val ApiVersion = "1.0"


  trait Decoder[A] {
    def decode(v: Vector[Json]): Either[DecodingFailure, A]
  }

  object Decoder {
    def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev
    class DecodingFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause)
  }

}
