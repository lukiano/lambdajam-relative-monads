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

import scalaz._, Scalaz._

import scalaz.scalacheck.ScalazProperties.{monad, equal, plus}

import Arbitraries._

class ResultSpec extends Test { def is = s2"""
Result
======

Result should:
  obey monad laws                                                              ${monad.laws[Result]}
  obey equal laws                                                              ${equal.laws[Result[Int]]}
  obey plus laws                                                               ${plus.laws[Result]}
  toEither should roundtrip                                                    $toEitherRoundtrip
  getOrElse should always return value for Ok                                  $getOrElseOk
  getOrElse should always return else for Error                                $getOrElseError
  or returns first Ok                                                          $orFirstOk
  or skips first Error                                                         $orFirstError
  setMessage on Ok is noop                                                     $setMessageOk
  setMessage on Error always sets message                                      $setMessageError
  addMessage on Ok is noop                                                     $addMessageOk
  addMessage on Error always adds message, prepending to any existing messages $addMessageError

"""
  def toEitherRoundtrip = prop((x: Either[String, Int]) =>
    x.fold(Result.error, Result.ok).toEither must_== x)

  def toEither = prop((x: Int) =>
    Result.ok(x).toEither must beRight(x)
  )

  def getOrElseOk = prop((x: Int, y: Int) =>
    Result.ok(x).getOrElse(y) must_== x
  )

  def getOrElseError = prop((x: String, y: Int) =>
    Result.error(x).getOrElse(y) must_== y
  )

  def orFirstOk = prop((x: Int, y: Result[Int]) =>
    (Result.ok(x) or y) must_== Result.ok(x)
  )

  def orFirstError = prop((x: String, y: Result[Int]) =>
    (Result.error(x) or y) must_== y
  )

  def setMessageOk = prop((x: Int, message: String) =>
    Result.ok(x).setMessage(message) must_== Result.ok(x)
  )

  def setMessageError = prop((x: String, message: String) =>
    Result.error(x).setMessage(message).toEither must beLeft(message)
  )

  def addMessageOk = prop((x: Int, message: String) =>
    Result.ok(x).addMessage(message) must_== Result.ok(x)
  )

  def addMessageError = prop((x: String, message: String) =>
    Result.error(x).addMessage(message, ": ").toEither must beLeft(s"$message:$x")
  )
}
