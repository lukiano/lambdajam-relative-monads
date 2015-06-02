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

import scalaz.Scalaz._
import scalaz.Monad

object RelResult {
  @inline def apply[R[_]](implicit RM: RelMonad[Result, R]): RelMonad[Result, R] = RM
}


final class RelResultOps[R[_], A](val self: R[A])(implicit RM: RelMonad[Result, R]) {
  import RelResultSyntax._
  val M = Monad[Result]

  def rMap[B](f: Result[A] => Result[B]): R[B] = RM.rBind(self) { resultA =>
    Result.ok(RM.rPoint(f(resultA)))
  }

  def rFlatMap[B](f: Result[A] => R[B]): R[B] = RM.rBind(self) { resultA =>
    Result.ok(f(resultA))
  }

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    *
    * NB: This discards any existing message.
    */
  def rSetMessage(message: String): R[A] = rMap(_.setMessage(message))

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def rAddMessage(message: String, separator: String = ": "): R[A] = rMap(_.addMessage(message, separator))

  /**
    * Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
    *
    * Returns the error of `self` iff both `self` and `other` fail.
    */
  def rOr(other: R[A]): R[A] = RM.rBind(self) { resultA =>
    Result.ok(RM.rPoint(resultA)).or(Result.ok(other))
  }

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def rOnException[B](action: R[B]): R[A] = self.rFlatMap { resultA =>
    resultA.fold(a => RM.rPoint(Result.ok(a)), error => {
      action.rMap(_.fold(_ => Result.error(error), _ => Result.error(error)))
    })
  }

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def rEnsure[B](sequel: R[B])(implicit R: Monad[R]): R[A] = self.rFlatMap { resultA =>
    resultA.fold(a => {
      sequel.rMap(_.fold(_ => Result.ok(a), error => Result.error(error)))
    }, error => {
      sequel.rMap(_.fold(_ => Result.error(error), _ => Result.error(error)))
    })
  }

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    *
    * All errors are rethrown. Generalizes try/finally.
    */
  def rBracket[B, C](after: A => R[B])(during: A => R[C])(implicit R: Monad[R]): R[C] = self.rFlatMap { resultA =>
    resultA.fold(a => during(a).rEnsure(after(a)), error => RM.rPoint(Result.error(error)))
  }
}

/** Pimps a [[RelResult]] to have access to the functions in [[RelResultOps]].
  *
  * The usual use of this is to mix it into the companion object to ensure the implicit resolution
  * priority does not clash with Scalaz.
  */
trait ToRelResultOps {
  /** Pimps a [[RelResult]] to have access to the functions in [[RelResultOps]]. */
  implicit def ToRelResultOps[R[_], A](v: R[A])(implicit M0: RelMonad[Result, R]): RelResultOps[R, A] =
    new RelResultOps[R, A](v)
}

/** Pimps a [[RelResult]] to have access to the functions in [[RelResultOps]]. */
object RelResultSyntax extends ToRelResultOps
