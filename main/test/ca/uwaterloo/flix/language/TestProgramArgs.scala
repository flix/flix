/*
 * Copyright 2022 Jonathan Lindegaard Starup
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{Options, Result, Validation}
import org.scalatest.funsuite.AnyFunSuite

class TestProgramArgs extends AnyFunSuite with TestUtils {

  test("ProgramArgs.01") {
    val arg = "Correct"
    val input =
      s"""
        |def main(): Unit = match Environment.getArgs() {
        |    case "$arg" :: Nil => ()
        |    case _ :: Nil => ?wrongArgumentValue
        |    case _ => ?incorrectNumberOfArgs
        |}
      """.stripMargin
    val compilationValidation = compile(input, Options.TestWithLibAll)
    compilationValidation.toResult match {
      case Result.Ok((result, Chain.empty)) => result.getMain match {
        case Some(main) => try {
          main.apply(Array(arg))
        } catch {
          case e: java.lang.Throwable => fail(e)
        }
        case None => fail("No entrypoint")
      }
      case Result.Ok((_, failures)) =>
        val actuals = failures.map(_.getClass)
        fail(s"Expected success, but found errors ${actuals.mkString(", ")}.")
      case Result.Err(failures) =>
        val actuals = failures.map(_.getClass)
        fail(s"Expected success, but found errors ${actuals.mkString(", ")}.")
    }
  }

}
