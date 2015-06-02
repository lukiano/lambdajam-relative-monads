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

import lambdajam.FST.FST

import scalaz.Monad

case class FS[A](fst: FST[A]) {
  def runPath(s: String): Result[A] = runFS(new File(s))
  val runFS: File => Result[A] = fst.run
  import FST._

  def map[B](f: A => B): FS[B] = fst.map(f).toFS /* FS {
    runFS(_) map f
  }*/

  def flatMap[B](f: A => FS[B]): FS[B] = fst.flatMap(f.andThen(_.fst)).toFS /* FS {
    file => runFS(file).fold(a => f(a).runFS(file), error => Result.error(error))
  }*/

  /**
    * Set the error message in a failure case. Useful for providing contextual information without
    * having to inspect result.
    *
    * NB: This discards any existing message.
    */
  def setMessage(message: String): FS[A] = fst.tSetMessage(message).toFS /* FS {
    runFS(_).setMessage(message)
  }*/

  /**
    * Adds an additional error message. Useful for adding more context as the error goes up the stack.
    *
    * The new message is prepended to any existing message.
    */
  def addMessage(message: String): FS[A] = fst.tAddMessage(message).toFS /* FS {
    runFS(_).addMessage(message)
  }*/

  /**
    * Runs the first operation. If it fails, runs the second operation. Useful for chaining optional operations.
    *
    * Returns the error of `self` iff both `self` and `other` fail.
    */
  def or(other: FS[A]): FS[A] = fst.tOr(other.fst).toFS /* FS {
    file => runFS(file).or(other.runFS(file))
  }*/

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def onException[B](sequel: FS[B]): FS[A] = fst.tOnException(sequel.fst).toFS /* FS {
    file => runFS(file).fold(Result.ok, error => {
      sequel.runFS(file)
      Result.error(error)
    })
  }*/

  /**
    * Ensures that the provided action is always run regardless of if `this` was successful.
    * Generalizes "finally".
    *
    * If `self` was successful and `sequel` fails it returns the failure from `sequel`. Otherwise
    * the result of `self` is returned.
    */
  def ensure[B](sequel: FS[B]): FS[A] = fst.tEnsure(sequel.fst).toFS /* FS {
    file => runFS(file).fold(a => {
      sequel.runFS(file).fold(_ => Result.ok(a), Result.error )
    }, error => {
      sequel.runFS(file)
      Result.error(error)
    })
  } */

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    *
    * All errors are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => FS[B])(during: A => FS[C]): FS[C] = fst.tBracket(after.andThen(_.fst))(during.andThen(_.fst)).toFS /* FS {
    file => runFS(file).fold(a => {
      val fsc = during(a)
      val fsb = after(a)
      fsc.runFS(file).fold(c => {
        fsb.runFS(file).fold(_ => Result.ok(c), Result.error)
      }, Result.error)
    }, error => {
      Result.error[C](error)
    })
  }*/
}

object FS extends ToRelResultOps {
  def fs[A](a: => A): FS[A] = apply {
    _ => Result.safe(a)
  }

  def apply[A](r: File => Result[A]): FS[A] = FS(FST(r))

  /** Lists the files in a directory but doesn't have a nice error message. */
  def listFiles: FS[List[String]] = apply(f => Result.safeNull(f.list).map(_.toList))

  /** List files but with a nice error message. */
  def ls: FS[List[String]] = listFiles.setMessage("File is not a directory!")

  implicit def FSMonad: Monad[FS] = new Monad[FS] {
    def point[A](v: => A) = fs(v)
    def bind[A, B](a: FS[A])(f: A => FS[B]) = a flatMap f
  }


  /************** Relative Monad instance for FS relative to Result *************/

  import FST._

  implicit val relMonad: RelMonad[Result, FS] = new RelMonad[Result, FS] {
    /** Similar to a `Monad.point` but expects a `Result`. */
    def rPoint[A](v: => Result[A]): FS[A] = apply {
      _ => v
    }

    /** Similar to a `Monad.bind` but expects a `Result`. */
    def rBind[A, B](ma: FS[A])(f: Result[A] => Result[FS[B]]): FS[B] = ma.flatMap { a =>
      f(Result.ok(a)).fold(identity, error => MonadResult[FST].raiseError[B](error).toFS)
    }
  }

  /** List files but with a nice error message using RelResult functions. */
  def rLS: FS[List[String]] = FS.listFiles.rSetMessage("File is not a directory!")
}
