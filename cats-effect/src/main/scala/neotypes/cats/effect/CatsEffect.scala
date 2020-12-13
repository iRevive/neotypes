package neotypes.cats.effect

import cats.effect.{Concurrent, ExitCase, Resource}

trait CatsEffect {
  private[neotypes] final type FResource[F[_]] = { type R[A] = Resource[F, A] }

  implicit final def catsAsync[F[_]](implicit F: Concurrent[F]): neotypes.Async.Aux[F, FResource[F]#R] =
    new neotypes.Async[F] {
      override final type R[A] = Resource[F, A]

      override private[neotypes] def async[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
        F.async(cb)

      override private[neotypes] def delay[A](a: => A): F[A] =
        F.delay(a)

      override private[neotypes] def map[A, B](fa: F[A])(f: A => B): F[B] =
        F.map(fa)(f)

      override private[neotypes] def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
        F.flatMap(fa)(f)

      override private[neotypes] def fromEither[A](e: => Either[Throwable, A]): F[A] =
        F.fromEither(e)

      override private[neotypes] def guarantee[A, B](fa: F[A])(f: A => F[B])(finalizer: (A, Option[Throwable]) => F[Unit]): F[B] =
        Resource.makeCase(fa) {
          case (a, ExitCase.Completed | ExitCase.Canceled) => finalizer(a, None)
          case (a, ExitCase.Error(ex))                     => finalizer(a, Some(ex))
        }.use(f)

      override private[neotypes] def resource[A](a: => A)(close: A => F[Unit]): Resource[F, A] =
        Resource.make(delay(a))(close)
    }

}
