package matwojcik.movies.util

import au.id.tmm.bfect.effects._
import cats.~>
import au.id.tmm.bfect.effects.Die._

object BifunctorImplicits {

  implicit class RefineErrors[F[+_,+_]: Sync, E <: Throwable, A](fa: F[E, A]) {
    def refineError[E2](throwableToE: E => E2): F[E2,A] =
      fa.refineOrDie{
        case failure => throwableToE(failure)
      }
  }

  implicit class LiftToRefine[E](throwableToE: Throwable => E) {
    def liftToRefineK[F[+_,+_]: Sync]: ~>[F[Throwable, ?], F[E, ?]] = λ[F[Throwable, ?] ~> F[E, ?]](_.refineError(throwableToE))
  }
}
