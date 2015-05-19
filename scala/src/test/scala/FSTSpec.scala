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

//import scalaz.Scalaz._
import scalaz.{Equal, Monad}
import scalaz.scalacheck.ScalazProperties.{monad, equal, plus}

import org.specs2.matcher.Matcher
import org.specs2.execute.{Result => SpecResult}

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import FST._//FST
//import MonadResult._
//import MonadResultSyntax._
import Arbitraries._

class FSTSpec extends Test { def is = s2"""
Using FST
=========

MonadResult functions for FST should:
  allow setting an error message                                                             $setMessage
  allow adding an error message                                                              $addMessage
  onException does not change the result and performs the provided action on error           $onException
  ensure always perform the expected action                                                  $ensureAlwaysAction
  ensure fails if the action fails and returns either the original error or the action error $ensureError
  bracket is syntactic sugar for `ensure` and `flatMap`                                      $bracket


"""

  implicit val a = implicitly[MonadResult[FST]]

  def setMessage = prop((x: Result[Int], msg: String) =>
    result(x).tSetMessage(msg) must beResult(x.setMessage(msg))
  )
  
  def addMessage = prop((x: Result[Int], msg: String) =>
    result(x).tAddMessage(msg) must beResult(x.addMessage(msg))
  )

  def onException = prop ((x: FST[Int]) => {
    var flag = false

    val actual = x.tOnException(FST(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    actual must beResultLike {
      case Ok(_)    => flag must beFalse
      case Error(_) => flag must beTrue
    }
  })

  def ensureAlwaysAction = prop ((x: FST[Int]) => {
    var flag = false
    val actual = x.tEnsure(FST(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    flag must beTrue

  })

  def ensureError = prop ((x: FST[Int], y: FST[Int]) => {
    x.tEnsure(y) must equal (x.flatMap(_ => y.flatMap(_ => x)))
  })

  def bracket = prop ((x: FST[Int], y: FST[Int], z: FST[Int]) =>
    x.tBracket(_ => y)(_ => z) must equal (x.flatMap(_ => z).tEnsure(y))
  )

  def result[A](r: Result[A]): FST[A] = FST(_ => r)

  /* Matchers */
  def beResult[A](expected: Result[A]): Matcher[FST[A]] =
    (h: FST[A]) => h.run(new File(".")) must_== expected

  def beResultLike[A](expected: Result[A] => SpecResult): Matcher[FST[A]] =
    (h: FST[A]) => expected(h.run(new File(".")))

  def beValue[A](expected: A): Matcher[FST[A]] =
    beResult(Result.ok(expected))

  def equal[A : Equal](expected: A): Matcher[A] =
    (h: A) => implicitly[Equal[A]].equal(h, expected)

  /* Note these are not general purpose, specific to testing laws. */

  implicit def FSTEqual: Equal[FST[Int]] =
    Equal.equal[FST[Int]]((a, b) =>
      a.run(new File(".")) must_== b.run(new File(".")))

  implicit def FSTAribtrary[A : Arbitrary]: Arbitrary[FST[A]] =
    Arbitrary(arbitrary[Result[A]] map (r => FST(_ => r)))
}
