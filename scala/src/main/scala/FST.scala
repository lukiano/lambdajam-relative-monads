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

import scalaz.{KleisliFunctions, Monad, ReaderT}

object FST extends KleisliFunctions {
  type FST[A] = ReaderT[Result, File, A]

  def apply[A](f: File => Result[A]): FST[A] = kleisli(f)
  def ok[A](a: => A)                         = Monad[FST].point(a)
  def error[A](e: String): FST[A]            = MonadResult.error[FST, A](e)

  def listFiles: FST[List[String]] = apply(f => Result.safeNull(f.list).map(_.toList))

  /** List files but with a nice error message using MonadResult functions. */
  def ls: FST[List[String]] = ???

  implicit def FSTMonadResultOps[A](v: FST[A]): MonadResultOps[FST, A] =
    new MonadResultOps[FST, A](v)
}
