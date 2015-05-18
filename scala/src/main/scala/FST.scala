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

import scalaz._, Scalaz._

object FST {
  type FST[A] = ReaderT[Result, File, A]

  implicit def readerTMonadResult[F[_], R](implicit M0: Monad[F]): MonadResult[({ type λ[α] = ReaderT[F, R, α] })#λ] = new ReaderTMonadResult[F, R] {
    implicit def M: Monad[F] = M0
  }

  trait ReaderTMonadResult[F[_], R] extends MonadResult[({ type λ[α] = ReaderT[F, R, α] })#λ] {
    implicit def M: Monad[F]

    def raiseError[A](e: String): ReaderT[F, R, A] = ???
    def handleError[A](ma: ReaderT[F, R, A])(f: String => ReaderT[F, R, A]): ReaderT[F, R, A] = ???

    def point[A](a: => A): ReaderT[F, R, A] = ???//(M.point(a))
    def bind[A, B](ma: ReaderT[F, R, A])(f: A => ReaderT[F, R, B]): ReaderT[F, R, B] = ???
  }
}

trait MonadResult[M[_]] extends Monad[M] {
  def raiseError[A](e: String): M[A]
  def handleError[A](ma: M[A])(f: String => M[A]): M[A]
}

object MonadResult {
  def apply[F[_]](implicit M: MonadResult[F]) = M
}
