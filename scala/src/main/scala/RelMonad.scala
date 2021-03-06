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

trait RelMonad[M[_], R[_]]{
  def rPoint[A](v: => M[A]): R[A]
  def rBind[A, B](ma: R[A])(f: M[A] => M[R[B]]): R[B]
}

object RelMonad {
  @inline def apply[M[_], R[_]](implicit RM: RelMonad[M, R]): RelMonad[M, R] = RM
}
