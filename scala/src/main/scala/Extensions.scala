package lambdajam

import scala.concurrent.Future

import java.io.File

import scalaz._, Scalaz._

object Extensions {

  def tMap[M[_], A, B](ma: M[A])(f: Result[A] => Result[B])(implicit M: MonadResult[M]): M[B] = ???

  def relMonad[R[_], M[_]](implicit M: Monad[M], RM: RelMonad[M, R]): Monad[R] = new Monad[R] {
    def point[A](v: => A) = ???
    def bind[A, B](ma: R[A])(f: A => R[B]): R[B] = ???
  }

  val fsRelReader: RelMonad[({ type λ[α] = Reader[File, α] })#λ, FS] = ???

  val futureRelResult: RelMonad[Result, Future] = ???
}

object RelReader {
  @inline def apply[R[_], X](implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]) = RM

  def rAsk[R[_], X, A](implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]): R[A] = ???
}

final class RelReaderOps[R[_], X, A](val self: R[A])(implicit RM: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]) {
  def rLocal(f: X => X): R[A] = ???
}

trait ToRelReaderOps {
  implicit def ToRelReaderOps[R[_], X, A](v: R[A])(implicit M0: RelMonad[({ type λ[α] = Reader[X, α] })#λ, R]): RelReaderOps[R, X, A] =
    new RelReaderOps[R, X, A](v)
}

object RelReaderSyntax extends ToRelReaderOps
