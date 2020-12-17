package neotypes

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

@annotation.implicitNotFound("The effect type ${F} is not supported by neotypes")
trait Async[F[_]] {
  private[neotypes] type R[A]

  private[neotypes] def async[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A]

  private[neotypes] def delay[A](a: => A): F[A]

  private[neotypes] def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  private[neotypes] def fromEither[A](e: => Either[Throwable, A]): F[A]

  private[neotypes] def guarantee[A, B](fa: F[A])
                                       (f: A => F[B])
                                       (finalizer: Outcome[A] => F[Unit]): F[B]

  private[neotypes] def map[A, B](fa: F[A])(f: A => B): F[B]

  private[neotypes] def resource[A](a: => A)(close: A => F[Unit]): R[A]
}


sealed abstract class Outcome[+A] extends Product with Serializable

object Outcome {
  final case class Completed[A](a: A) extends Outcome[A]
  final case class Error[A](a: A, e: Throwable) extends Outcome[A]
  final case class Canceled[A](a: A) extends Outcome[A]

  def complete[A](v: A): Outcome[A] = Completed(v)
  def error[A](v: A, e: Throwable): Outcome[A] = Error(v, e)
  def canceled[A](v: A): Outcome[A] = Canceled(v)
}

object Async {
  type Aux[F[_], _R[_]] = Async[F] { type R[A] = _R[A] }

  private[neotypes] type Id[A] = A

  implicit def futureAsync(implicit ec: ExecutionContext): Async.Aux[Future, Id] =
    new Async[Future] {
      override final type R[A] = A

      override final def async[A](cb: (Either[Throwable, A] => Unit) => Unit): Future[A] =
        Future {
          val p = Promise[A]()
          cb {
            case Right(res) => p.complete(Success(res))
            case Left(ex) => p.complete(Failure(ex))
          }
          p.future
        }.flatten

      override final def delay[A](a: => A): Future[A] =
        Future(a)

      override final def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] =
        fa.flatMap(f)

      override final def fromEither[A](e: => Either[Throwable,A]): Future[A] =
        Future.fromTry(e.toTry)

      override final def guarantee[A, B](fa: Future[A])
                                        (f: A => Future[B])
                                        (finalizer: Outcome[A] => Future[Unit]): Future[B] =
        fa.flatMap { a =>
          f(a).transformWith {
            case Success(b)  => finalizer(Outcome.complete(a)).map(_ => b)
            case Failure(ex) => finalizer(Outcome.error(a, ex)).transform(_ => Failure(ex))
          }
        }

      override final def map[A, B](fa: Future[A])(f: A => B): Future[B] =
        fa.map(f)

      override final def resource[A](a: => A)(close: A => Future[Unit]): A =
        a
    }
}
