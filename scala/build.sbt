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

name := "lambdajam-relative-monads"

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-feature",
  "-language:_",
  "-target:jvm-1.7"
)


libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"               % "7.1.1",
  "org.specs2"     %% "specs2-core"               % "3.5"    % "test",
  "org.specs2"     %% "specs2-scalacheck"         % "3.5"    % "test" exclude("org.scalacheck", "scalacheck_2.11"),
  "org.scalaz"     %% "scalaz-scalacheck-binding" % "7.1.1"  % "test" exclude("org.scalacheck", "scalacheck_2.11"),
  "org.scalacheck" %% "scalacheck"                % "1.11.4" % "test"
)

updateOptions := updateOptions.value.withCachedResolution(true)
