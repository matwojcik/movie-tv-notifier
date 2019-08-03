package matwojcik.movies.util

import au.id.tmm.bfect.effects._
import cats.~>
import au.id.tmm.bfect.effects.Die._
import au.id.tmm.bfect.catsinterop._

object BifunctorImplicits {

  implicit class RefineErrors[F[+_,+_]: Sync, A](fa: F[Throwable, A]) {
    def refineError[E](throwableToE: Throwable => E): F[E,A] =
      fa.refineOrDie{
        case failure => throwableToE(failure)
      }
  }

  implicit class LiftToRefine[E](throwableToE: Throwable => E) {
    def liftToRefineK[F[+_,+_]: Sync]: ~>[F[Throwable, ?], F[E, ?]] = λ[F[Throwable, ?] ~> F[E, ?]](_.refineError(throwableToE))
  }
}
