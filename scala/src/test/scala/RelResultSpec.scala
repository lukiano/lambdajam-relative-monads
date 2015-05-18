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

import scala.util.Random

import java.io.File

import scalaz.Scalaz._
import scalaz.{Equal, Monad}
import scalaz.scalacheck.ScalazProperties.{monad, equal, plus}

import org.specs2.matcher.Matcher
import org.specs2.execute.{Result => SpecResult}

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import Arbitraries.ResultAribtrary

class RelResultSpec extends Test { def is = s2"""
RelResult
=========

RelResult functions should:
  allow setting an error message                                                             $setMessage
  allow adding an error message                                                              $addMessage
  onException does not change the result and performs the provided action on error           $onException
  ensure always perform the expected action                                                  $ensureAlwaysAction
  ensure fails if the action fails and returns either the original error or the action error $ensureError
  bracket is syntactic sugar for `ensure` and `flatMap`                                      $bracket


"""

  def setMessage = prop((x: Result[Int], msg: String) =>
    result(x).rSetMessage(msg) must beResult(x.setMessage(msg))
  )
  
  def addMessage = prop((x: Result[Int], msg: String) =>
    result(x).rAddMessage(msg) must beResult(x.addMessage(msg))
  )

  def onException = prop ((x: Resultant[Int]) => {
    var flag = false

    val actual = x.rOnException(Resultant(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    actual must beResultLike {
      case Ok(_)    => flag must beFalse
      case Error(_) => flag must beTrue
    }
  })

  def ensureAlwaysAction = prop ((x: Resultant[Int]) => {
    var flag = false
    val actual = x.rEnsure(Resultant(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    flag must beTrue

  })

  def ensureError = prop ((x: Resultant[Int], y: Resultant[Int]) => {
    x.rEnsure(y) must equal (x.flatMap(_ => y.flatMap(_ => x)))
  })

  def bracket = prop ((x: Resultant[Int], y: Resultant[Int], z: Resultant[Int]) =>
    x.rBracket(_ => y)(_ => z) must equal (x.flatMap(_ => z).rEnsure(y))
  )

  def result[A](r: Result[A]): Resultant[A] = Resultant(_ => r)


  /* Matchers */

  def beResult[A](expected: Result[A]): Matcher[Resultant[A]] =
    (h: Resultant[A]) => h.f(Random.nextInt) must_== expected

  def beResultLike[A](expected: Result[A] => SpecResult): Matcher[Resultant[A]] =
    (h: Resultant[A]) => expected(h.f(Random.nextInt))

  def beValue[A](expected: A): Matcher[Resultant[A]] =
    beResult(Result.ok(expected))

  def equal[A : Equal](expected: A): Matcher[A] =
    (h: A) => implicitly[Equal[A]].equal(h, expected)
}

/** Dumpy implementation of an instance of a ResultantMonad. */
case class Resultant[A](f: Int => Result[A]) {
  override def toString = s"Resultant(_ => ${f(1).toString})"
}

/** Arbitraries and typeclass implementations of [[Resultant]]. */
object Resultant extends ToRelResultOps {
  implicit val relMonad: RelMonad[Result, Resultant] = new RelMonad[Result, Resultant] {
    /** Similar to a `Monad.point` but expects a `Result`. */
    def rPoint[A](v: => Result[A]): Resultant[A] = Resultant(_ => v)

    /** Similar to a `Monad.bind` but expects a `Result`. */
    def rBind[A, B](ma: Resultant[A])(f: Result[A] => Result[Resultant[B]]): Resultant[B] =
      Resultant(x => f(ma.f(x)).fold(r => r.f(x), e => Result.error(e)))
  }

  implicit val monad: Monad[Resultant] = new Monad[Resultant] {
    def point[A](v: => A): Resultant[A] = Resultant(_ => Result.ok(v))
    def bind[A, B](ma: Resultant[A])(f: A => Resultant[B]): Resultant[B] = Resultant(
      i => ma.f(i).flatMap(a => f(a).f(i))
    )
  }

  implicit def ResultantEqual[A]: Equal[Resultant[A]] = {
    val i = Random.nextInt
    Equal.equal[Resultant[A]]((a, b) => a.f(i) == b.f(i))
  }

  implicit def ResultantArbitrary[A : Arbitrary]: Arbitrary[Resultant[A]] =
    Arbitrary(arbitrary[Result[A]].map(r => Resultant(_ => r)))
}

