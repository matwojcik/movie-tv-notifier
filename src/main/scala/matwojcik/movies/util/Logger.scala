package matwojcik.movies.util

import au.id.tmm.bfect.effects._
import cats.tagless.Derive
import io.chrisdavenport.log4cats.MessageLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import au.id.tmm.bfect.catsinterop._
import au.id.tmm.bfect.effects.Die._
import cats.~>
import BifunctorImplicits._

object Logger {
  def getLogger[F[+_, +_]: Sync: Bracket, E](throwableToE: Throwable => E): MessageLogger[F[E, ?]] =
    Derive.functorK[MessageLogger].mapK(Slf4jLogger.getLogger[F[Throwable, ?]])(throwableToE.liftToRefineK[F])
}
