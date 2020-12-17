package neotypes
package internal.syntax

private object async {
  implicit class AsyncOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit F: Async[F]): F[B] =
      F.map(fa)(f)

    def flatMap[B](f: A => F[B])(implicit F: Async[F]): F[B] =
      F.flatMap(fa)(f)

    def guarantee[B](f: A => F[B])(finalizer: Outcome[A] => F[Unit])(implicit F: Async[F]): F[B] =
      F.guarantee(fa)(f)(finalizer)

    def guarantee(finalizer: Option[Throwable] => F[Unit])(implicit F: Async[F]): F[A] =
      F.guarantee(F.delay(()))(_ => fa) {
        case Outcome.Error(_, ex) => finalizer(Some(ex))
        case _ => finalizer(None)
      }
  }
}
