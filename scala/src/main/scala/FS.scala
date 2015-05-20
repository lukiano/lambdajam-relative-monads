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

import java.io.File

import scalaz.Monad

case class FS[A](runFS: File => Result[A]) {
  def runPath(s: String): Result[A] = runFS(new File(s))

  def map[B](f: A => B): FS[B] =
    flatMap(f.andThen(FS.fs(_)))

  def flatMap[B](f: A => FS[B]) =
    FS[B]((cwd: File) => runFS(cwd).flatMap(a => f(a).runFS(cwd)))

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    *
    * NB: This discards any existing message.
    */
  def setMessage(message: String): FS[A] =
    FS((cwd: File) => runFS(cwd).setMessage(message))

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String): FS[A] =
    FS((cwd: File) => runFS(cwd).addMessage(message))

  /**
    * Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
    *
    * Returns the error of `self` iff both `self` and `other` fail.
    */
  def or(other: FS[A]): FS[A] =
    FS((cwd: File) => runFS(cwd) or other.runFS(cwd))

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def onException[B](sequel: FS[B]): FS[A] =
    FS((cwd: File) => runFS(cwd).fold(
      x => Result.ok(x),
      e => {
        sequel.runFS(cwd)
        Result.error(e)
      }
    ))

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def ensure[B](sequel: FS[B]): FS[A] = for {
    r <- onException(sequel)
    _  <- sequel
  } yield r

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    *
    * All errors are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => FS[B])(during: A => FS[C]): FS[C] = for {
    a <- this
    r <- during(a) ensure after(a)
  } yield r
}

object FS extends ToRelResultOps {
  def fs[A](a: => A): FS[A] = FS(_ => Result.safe(a))

  /** Lists the files in a directory but doesn't have a nice error message. */
  def listFiles: FS[List[String]] = FS(f => Result.safeNull(f.list).map(_.toList))

  /** List files but with a nice error message. */
  def ls: FS[List[String]] = listFiles.setMessage("Invalid path")

  implicit def FSMonad: Monad[FS] = new Monad[FS] {
    def point[A](v: => A) = fs(v)
    def bind[A, B](a: FS[A])(f: A => FS[B]) = a.flatMap(f)
  }

  implicit val relMonad: RelMonad[Result, FS] = new RelMonad[Result, FS] {
    /** Similar to a `Monad.point` but expects a `Result`. */
    def rPoint[A](v: => Result[A]): FS[A] = FS(_ => v)

    /** Similar to a `Monad.bind` but expects a `Result`. */
    def rBind[A, B](ma: FS[A])(f: Result[A] => Result[FS[B]]): FS[B] =
      FS(x => f(ma.runFS(x)).fold(r => r.runFS(x), e => Result.error(e)))
  }

  /** List files but with a nice error message using RelResult functions. */
  def rLS: FS[List[String]] = listFiles.rSetMessage("Invalid path")
}
