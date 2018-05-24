/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.api.{Flix, RuleException}
import ca.uwaterloo.flix.language.errors.{NonExhaustiveMatchError, ResolutionError}
import ca.uwaterloo.flix.runtime.Model
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestParser extends FunSuite with TestUtils {

  /**
    * Runs Flix on the given input string `s`.
    */
  def run(s: String, core: Boolean = true): Model = {
    new Flix().setOptions(Options.DefaultTest.copy(core = core)).addStr(s).solve().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////

  test("Declaration.Rule.01") {
    val input =
      """
        |rel R(a: Int)
        |
        |R(x) :- R(x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.02") {
    val input =
      """
        |rel R(a: Int)
        |
        |R(x) :- R(x), R(x), R(x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.03") {
    val input =
      """
        |rel R(a: Int, b: Int)
        |
        |R(x, y) :- R(x, y), R(y, x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.04") {
    val input =
      """def f(): Int = 42
        |
        |rel R(a: Int)
        |
        |R(f()).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.Head.True") {
    val input = "true."
    run(input)
  }

  test("Declaration.Fact.Head.False") {
    intercept[RuleException] {
      val input = "false."
      run(input)
    }
  }

  test("Declaration.Rule.Head.True") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |true :- R(x, y).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.Head.False") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |false :- R(x, y).
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.01") {
    val input =
      """
        |def f(): Int =
        |  let x = 42;
        |    x
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.02") {
    val input =
      """
        |def f(): Int =
        |  let (x, y) = (42, 21);
        |    x + y
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.03") {
    val input =
      """
        |def f(): Int =
        |  let x = 1;
        |  let y = 2;
        |  let z = 3;
        |    x + y + z
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.04") {
    // Note: This is to test the performance of deeply nested lets.
    val input =
      """
        |def f(): Int =
        |    let x1 = 1;
        |    let x2 = 1;
        |    let x3 = 1;
        |    let x4 = 1;
        |    let x5 = 1;
        |    let x6 = 1;
        |    let x7 = 1;
        |    let x8 = 1;
        |    let x9 = 1;
        |    let y1 = 1;
        |    let y2 = 1;
        |    let y3 = 1;
        |    let y4 = 1;
        |    let y5 = 1;
        |    let y6 = 1;
        |    let y7 = 1;
        |    let y8 = 1;
        |    let y9 = 1;
        |    let z1 = 1;
        |    let z2 = 1;
        |    let z3 = 1;
        |    let z4 = 1;
        |    let z5 = 1;
        |    let z6 = 1;
        |    let z7 = 1;
        |    let z8 = 1;
        |    let z9 = 1;
        |        1
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.05") {
    val input =
      """
        |def f(): Int =
        |  let x = 42;
        |    x
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.06") {
    val input =
      """
        |def f(): Int =
        |  let x = 42;
        |  let y = 21;
        |    x + y
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.07") {
    val input =
      """
        |def f(): Int =
        |  let x = 1;
        |  let y = 2;
        |  let z = 3;
        |    x + y + z
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Red => 1
        |  case Blu => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Color.Red => 1
        |  case Color.Blu => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.03") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Rectangle(Int, Int)
        |}
        |
        |def f(x: Shape): Int = match x with {
        |  case Circle(r) => r
        |  case Rectangle(h, w) => h * w
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.04") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Rectangle(Int, Int)
        |}
        |
        |def f(x: Shape): Int = match x with {
        |  case Shape.Circle(r) => r
        |  case Shape.Rectangle(h, w) => h * w
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Tuple.01") {
    val input =
      """def f(x: (Bool, Char, Int)): Int = match x with {
        |  case (true, 'a', 42) => 1
        |  case (false, 'b', 21) => 2
        |  case _ => 3
        |}
      """.stripMargin
    run(input)
  }

}
