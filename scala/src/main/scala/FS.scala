//   Copyright 2014 Commonwealth Bank of Australia
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
  def map[B](f: A => B): FS[B] =
    flatMap(f.andThen(FS.fs(_)))

  def flatMap[B](f: A => FS[B]) =
    FS[B]((cwd: File) => runFS(cwd).flatMap(a => f(a).runFS(cwd)))

  def setMessage(message: String): FS[A] =
    FS((cwd: File) => runFS(cwd).setMessage(message))

  def addMessage(message: String): FS[A] =
    FS((cwd: File) => runFS(cwd).addMessage(message))

  def or(other: FS[A]): FS[A] =
    FS((cwd: File) => runFS(cwd) or other.runFS(cwd))

  def onException[B](sequel: FS[B]): FS[A] =
    FS((cwd: File) => runFS(cwd).fold(
      x => Result.ok(x),
      e => {
        sequel.runFS(cwd)
        Result.error(e)
      }
    ))

  def ensure[B](sequel: FS[B]): FS[A] = for {
    r <- onException(sequel)
    _  <- sequel
  } yield r

  def bracket[B, C](after: A => FS[B])(during: A => FS[C]): FS[C] = for {
    a <- this
    r <- during(a) ensure after(a)
  } yield r
}

object FS {
  def fs[A](a: => A): FS[A] = FS(_ => Result.safe(a))

  implicit def FSMonad: Monad[FS] = new Monad[FS] {
    def point[A](v: => A) = fs(v)
    def bind[A, B](a: FS[A])(f: A => FS[B]) = a.flatMap(f)
  }
}
