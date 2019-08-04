package matwojcik.movies.bfect

import au.id.tmm.bfect.Bifunctor
import au.id.tmm.bfect.effects.Die._
import au.id.tmm.bfect.effects.Sync
import cats.{Parallel, ~>}

object implicits {

  implicit class RefineErrors[F[+_,+_]: Sync, E <: Throwable, A](fa: F[E, A]) {
    def refineError[E2](throwableToE: E => E2): F[E2,A] =
      fa.refineOrDie{
        case failure => throwableToE(failure)
      }
  }

  implicit class LiftToRefine[E](throwableToE: Throwable => E) {
    def liftToRefineK[F[+_,+_]: Sync]: ~>[F[Throwable, ?], F[E, ?]] = λ[F[Throwable, ?] ~> F[E, ?]](_.refineError(throwableToE))
  }

  implicit def bifunctorParallelToParallel[F[+_,+_], G[+_,+_], E](implicit P: BifunctorParallel[F, G]): Parallel[F[E, ?], G[E, ?]] = P.parallel[E]

  implicit class IdSyncOps[A](a: A) {
    def biPure[F[+_,+_]: Sync]: F[Nothing, A] = Sync[F].pure(a)
  }

  implicit def bfectBifunctorToCatsBifunctor[F[+_, +_] : Bifunctor]: cats.Bifunctor[F] = new CatsBifunctorForBfectBifunctor[F]()

  private class CatsBifunctorForBfectBifunctor[F[+_, +_]](implicit bfectBifunctor: Bifunctor[F]) extends cats.Bifunctor[F] {
    override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = bfectBifunctor.biMap(fab)(f, g)

    override def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bfectBifunctor.leftMap(fab)(f)
  }
}
