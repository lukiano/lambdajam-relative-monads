//   Copyright 2015 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package lambdajam

import scalaz._, Scalaz._

trait MonadResult[M[_]] extends Monad[M] {
  def raiseError[A](e: String): M[A]
  def handleError[A](ma: M[A])(f: String => M[A]): M[A]
}

object MonadResult extends KleisliFunctions {
  def apply[M[_]](implicit M0: MonadResult[M])                     = M0
  def ok[M[_], A](a: A)(implicit M0: MonadResult[M])               = M0.point(a)
  def error[M[_], A](e: String)(implicit M0: MonadResult[M]): M[A] = M0.raiseError(e)

  implicit def readerTMonadResult[F[_], R](implicit M0: MonadResult[F]): MonadResult[({ type λ[α] = ReaderT[F, R, α] })#λ] = 
    new ReaderTMonadResult[F, R] {
      implicit def M: MonadResult[F] = M0
    }

  trait ReaderTMonadResult[F[_], R] extends MonadResult[({ type λ[α] = ReaderT[F, R, α] })#λ] {
    implicit def M: MonadResult[F]

    def raiseError[A](e: String): ReaderT[F, R, A] = kleisli(_ => M.raiseError(e))

    def handleError[A](ma: ReaderT[F, R, A])(f: String => ReaderT[F, R, A]): ReaderT[F, R, A] =
      kleisli(r => M.handleError(ma.run(r))(x => f(x).run(r)))

    def point[A](a: => A): ReaderT[F, R, A] = kleisli(_ => M.point(a))

    def bind[A, B](ma: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] = ma.flatMap(f)
  }

  implicit def resultMonadResult: MonadResult[Result] = new MonadResult[Result] {
    def raiseError[A](e: String): Result[A] = Result.error(e)

    def handleError[A](ma: Result[A])(f: String => Result[A]): Result[A] = ma.fold(Result.ok, f)

    def point[A](a: => A): Result[A] = Result.ok(a)

    def bind[A, B](ma: Result[A])(f: A => Result[B]): Result[B] = ma.flatMap(f)
  }
}


final class MonadResultOps[M[_], A](val self: M[A])(implicit M: MonadResult[M]) {
  import MonadResultSyntax._

  def handleError(f: String => M[A]): M[A] = M.handleError(self)(f)

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    *
    * NB: This discards any existing message.
    */
  def tSetMessage(message: String): M[A] = handleError(_ => M.raiseError(message))

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def tAddMessage(message: String, separator: String = ": "): M[A] = 
    handleError(m => M.raiseError(s"${message}${separator}$m"))

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def tOnException[B](action: M[B]): M[A] =
    handleError(e => action.handleError(_ => M.raiseError(e)) >> M.raiseError(e))

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def tEnsure[B](sequel: M[B]): M[A] = for {
    r <- tOnException(sequel)
    _ <- sequel
  } yield r
  

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    *
    * All errors are rethrown. Generalizes try/finally.
    */
  def tBracket[B, C](after: A => M[B])(during: A => M[C]): M[C] = for {
    a <- self
    r <- during(a) tEnsure after(a)
  } yield r
}

trait ToMonadResultOps {
  /** Pimps a [[MonadResult]] to have access to the functions in [[MonadResultOps]]. */
  implicit def ToMonadResultOps[M[_], A](v: M[A])(implicit M0: MonadResult[M]): MonadResultOps[M, A] =
    new MonadResultOps[M, A](v)
}

/** Pimps a [[MonadResult]] to have access to the functions in [[MonadResultOps]]. */
object MonadResultSyntax extends ToMonadResultOps
