/*
 * Copyright 2015-2016 Jason Mittertreiner
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
import ca.uwaterloo.flix.language.errors.ExhaustiveMatchError
import org.scalatest.FunSuite
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.runtime.Model

class TestPatExhaustiveness extends FunSuite with TestUtils {

  /**
    * Runs Flix on the given input string `s`.
    */
  def run(s: String, core: Boolean = true): Model = {
    new Flix().setOptions(Options.DefaultTest.copy(core = core)).addStr(s).solve().get
  }
 /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Pattern.Wildcard.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case _ => 42
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Var.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case x => x
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Unit.01") {
    val input =
      """def f(x: Unit): Int = match x with {
        |  case () => 42
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Bool.01") {
    val input =
      """def f(x: Bool): Int = match x with {
        |  case true => 42
        |  case false => 21
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Char.01") {
    val input =
      """def f(x: Char): Int = match x with {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |}
      """.stripMargin
    expectError[ExhaustiveMatchError](new Flix().addStr(input).compile())
  }
  test("Pattern.Literal.Char.02") {
    val input =
      """def f(x: Char): Int = match x with {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int32.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case 1 => 1
        |  case 2 => 2
        |  case 3 => 3
        |}
      """.stripMargin
    expectError[ExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int = match x with {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |}
      """.stripMargin
    expectError[ExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    expectError[ExhaustiveMatchError](new Flix().addStr(input).compile())
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
}
