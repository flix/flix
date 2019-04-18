/*
 * Copyright 2018 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.SafetyError.IllegalNonPositivelyBoundVariable
import org.scalatest.FunSuite

class TestSafety extends FunSuite with TestUtils {

  test("NonPositivelyBoundVariable.01") {
    val input =
      """
        |rel A(x: Int)
        |rel B(x: Int)
        |
        |def main(): Schema { A, B, R } = solve {
        |    A(x) :- not B(x).
        |}
        |
      """.stripMargin
    expectError[IllegalNonPositivelyBoundVariable](new Flix().addStr(input).compile())
  }

  test("NonPositivelyBoundVariable.02") {
    val input =
      """
        |rel A(x: Int)
        |rel B(x: Int)
        |rel R(k: Int)
        |
        |def main(): Schema { A, B, R } = solve {
        |    R(x) :- A(x), not B(y).
        |}
        |
      """.stripMargin
    expectError[IllegalNonPositivelyBoundVariable](new Flix().addStr(input).compile())
  }

  test("NonPositivelyBoundVariable.03") {
    val input =
      """
        |rel A(x: Int)
        |rel B(x: Int)
        |rel R(k: Int)
        |
        |def main(): Schema { A, B, R } = solve {
        |    R(x) :- not A(x), not B(x).
        |}
        |
      """.stripMargin
    expectError[IllegalNonPositivelyBoundVariable](new Flix().addStr(input).compile())
  }

  test("NonPositivelyBoundVariable.04") {
    val input =
      """
        |rel A(x: Int)
        |rel B(x: Int)
        |rel R(k: Int)
        |
        |def main(): Schema { A, B, R } = solve {
        |    R(1) :- not A(x), not B(y).
        |}
        |
      """.stripMargin
    expectError[IllegalNonPositivelyBoundVariable](new Flix().addStr(input).compile())
  }

}
