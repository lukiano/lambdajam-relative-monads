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

import scalaz._, Scalaz._
import scalaz.scalacheck.ScalazProperties.{monad, equal, plus}

import org.specs2.matcher.Matcher
import org.specs2.execute.{Result => SpecResult}

import org.scalacheck.Arbitrary, Arbitrary.arbitrary

import Arbitraries._

class FSSpec extends Test with FSMatchers { def is = s2"""
FS
==

 FS should:
  obey monad laws                                                                            ${monad.laws[FS]}
  allow setting an error message                                                             $setMessage
  allow adding an error message                                                              $addMessage
  onException does not change the result and performs the provided action on error           $onException
  ensure always perform the expected action                                                  $ensureAlwaysAction
  ensure fails if the action fails and returns either the original error or the action error $ensureError
  bracket is syntactic sugar for `ensure` and `flatMap`                                      $bracket


"""

  def setMessage = prop((x: Result[Int], msg: String) =>
    result(x).setMessage(msg) must beResult(x.setMessage(msg))
  )
  
  def addMessage = prop((x: Result[Int], msg: String) =>
    result(x).addMessage(msg) must beResult(x.addMessage(msg))
  )

  def onException = prop ((x: FS[Int]) => {
    var flag = false

    val actual = x.onException(FS(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    actual must beResultLike {
      case Ok(_)    => flag must beFalse
      case Error(_) => flag must beTrue
    }
  })

  def ensureAlwaysAction = prop ((x: FS[Int]) => {
    var flag = false
    val actual = x.ensure(FS(_ => { flag = true; Result.ok(2) }))

    actual must equal(x)
    flag must beTrue

  })

  def ensureError = prop ((x: FS[Int], y: FS[Int]) => {
    x.ensure(y) must equal (x.flatMap(_ => y.flatMap(_ => x)))
  })

  def bracket = prop ((x: FS[Int], y: FS[Int], z: FS[Int]) =>
    x.bracket(_ => y)(_ => z) must equal (x.flatMap(_ => z).ensure(y))
  )

  def result[A](x: Result[A]): FS[A] = FS(_ => x)
}
