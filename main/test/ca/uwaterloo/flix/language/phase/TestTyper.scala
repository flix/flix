/*
 * Copyright 2020 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.TypeError
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  test("TestLeq01") {
    val input =
      """
        |def f(): a = 21
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq02") {
    val input =
      """
        |def f(): List[a] = 21 :: Nil
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq03") {
    val input =
      """
        |def f(): Result[a, Int] = Ok(21)
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq04") {
    val input =
      """
        |def f(): Result[Int, a] = Err(21)
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq05") {
    val input =
      """
        |def f(): a -> a = x -> 21
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq06") {
    val input =
      """
        |def f(): a -> a = (x: Int32) -> x
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq07") {
    val input =
      """
        |def f(): {x: Int | r} = {x = 21}
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

  test("TestLeq08") {
    val input =
      """
        |def f(): {x: Int, y: Int | r} = {y = 42, x = 21}
      """.stripMargin
    expectError[TypeError.GeneralizationError](new Flix().addStr(input).compile())
  }

}
