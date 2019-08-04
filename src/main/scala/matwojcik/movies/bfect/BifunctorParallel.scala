package matwojcik.movies.bfect

import cats.Parallel
import scalaz.zio.ZIO
import scalaz.zio.interop.ParIO
import scalaz.zio.interop.catz._

trait BifunctorParallel[F[+_,+_], G[+_,+_]] {
  def parallel[E]: Parallel[F[E, ?], G[E, ?]]
}

object BifunctorParallel {
  implicit def zioInstance: BifunctorParallel[ZIO[Any, +?, +?], ParIO[Any, +?, +?]] =
    new BifunctorParallel[ZIO[Any, +?, +?], ParIO[Any, +?, +?]] {
      override def parallel[E]: Parallel[ZIO[Any, E, ?], ParIO[Any, E, ?]] = parallelInstance[Any, E]
    }
}
