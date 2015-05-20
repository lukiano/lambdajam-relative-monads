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

import scala.util.control.NonFatal

import scalaz.{Equal, Monad, Plus}

/**
  * A data-type useful for representing the result of a possibly failing operation.
  * 
  * The Error result of an operation represents an initial error (either an error message or an
  * exception) with the option to add more details in the form of error messages.
  * 
  * The reason this exists as an alternative to using the more general
  * `Either`, `\/`, `Validation`, `Try` data-types is that we want to
  * gain better inference (we gain most of this by specialising the
  * error case), better composition (compared with `Try`, we gain most
  * of this by not mixing side-effects with map/flat) and better
  * library support by adding specific combinators for our given
  * case.
  */
sealed trait Result[A] {
  /**
    * `Catamorphism` for result. Concise data deconstruction that can be used as an alternative to
    * pattern matching, providing stronger coverage checks.
    */
  @inline final def fold[X](
    ok: A => X,
    error: String => X
  ): X = this match {
    case Ok(value)    => ok(value)
    case Error(msg) => error(msg)
  }

  /** Map across the success case. */
  def map[B](f: A => B): Result[B] =
    flatMap(f andThen Result.ok)

  /** Bind through the success case, this is useful to chain potentially failing operations. */
  def flatMap[B](f: A => Result[B]): Result[B] =
    fold(f, Result.error)

  /** Convert to scala.Either, this is useful for interop. */
  def toEither: Either[String, A] =
    fold(Right.apply, Left.apply)

  /** In the success case, get the value, otherwise return `els`, useful for defauling in error case. */
  def getOrElse(els: => A): A =
    fold(identity, _ => els)

  /** Take the first successful result. Useful for chaining optional operations. */
  def or(other: => Result[A]): Result[A] =
    fold(Result.ok, _ => other)

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    * 
    * NB: This discards any existing message.
    */
  def setMessage(message: String): Result[A] =
    fold(
      r => Result.ok(r),
      _ => Result.error(message)
    )

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    * 
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String, separator: String = ": "): Result[A] =
    fold(
      r      => Result.ok(r),
      m      => Result.error(s"${message}${separator}$m")
    )
}

/**
  * Successful `Result`, public so it can be used for pattern matching, prefer Result.ok for
  * construction.
  */
case class Ok[A](value: A) extends Result[A]

/**
  * Failing `Result`, public so it can be used for pattern matching, prefer Result.error for
  * construction.
  */
case class Error[A](error: String) extends Result[A]

object Result {
  /** Exception safe `Result` creation. */
  def safe[A](thunk: => A): Result[A] =
    try ok(thunk) catch { case NonFatal(t) => error(t.getMessage) }

  /** Exception and null safe `Result` creation. */
  def safeNull[A](thunk: => A): Result[A] =
    safe(thunk).flatMap(a => if (a == null) Result.error("Got null") else Result.ok(a))
  /**
    * Smart constructor for a successful `Result`. Provides better inference then direct use of
    * constructor. */
  def ok[A](value: A): Result[A] =
    Ok[A](value)

  /** Smart constructor for a failing case with a message. */
  def error[A](message: String): Result[A] =
    Error[A](message)

  /** scalaz Monad instance for Result. */
  implicit def ResultMonad: Monad[Result] = new Monad[Result] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](a: Result[A])(f: A => Result[B]) = a flatMap f
  }

  /** scalaz Equal instance for Result. */
  implicit def ResultEqual[A : Equal]: Equal[Result[A]] = new Equal[Result[A]] {
    def equal(r1: Result[A], r2: Result[A]) = (r1, r2) match {
      case (Ok(a1), Ok(a2))       => a1 == a2
      case (Error(m1), Error(m2)) => m1 == m2
      case _                      => false
    }
  }

  /** scalaz Plus instance for Result. */
  implicit def ResultPlus: Plus[Result] = new Plus[Result] {
    def plus[A](a: Result[A], b: => Result[A]): Result[A] =
      a or b
  }
}
