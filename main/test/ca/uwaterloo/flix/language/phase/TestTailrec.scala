/*
 * Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import org.scalatest.FunSuite

class TestTailrec extends FunSuite {

  test("Tailrec.01") {
    val input =
      """def r: Int = f(42)
        |
        |def f(i: Int): Int =
        |  if (i == 0) 0 else f(i - 1)
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.02") {
    val input =
      """def r: Int = f(42)
        |
        |def f(i: Int): Int =
        |  if (i == 21) 84 else f(i - 1)
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assertResult(84)(result.get.getConstant("r"))
  }

  test("Tailrec.03") {
    val input =
      """def r: Int = f(1000000)
        |
        |def f(i: Int): Int =
        |  if (i == 0) 0 else f(i - 1)
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assertResult(0)(result.get.getConstant("r"))
  }

  test("Tailrec.04") {
    val input =
      """def r: Int = f(42, 0)
        |
        |def f(i: Int, j: Int): Int =
        |  if (i == 0) j else f(i - 1, j + 1)
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assertResult(42)(result.get.getConstant("r"))
  }

  test("Tailrec.05") {
    val input =
      """def r: Int = f(42, 0)
        |
        |def f(i: Int, j: Int): Int =
        |  if (i == 0) j else f(i - 1, j + 2)
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assertResult(84)(result.get.getConstant("r"))
  }

}
