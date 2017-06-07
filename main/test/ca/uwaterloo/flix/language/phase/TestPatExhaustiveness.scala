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
import ca.uwaterloo.flix.language.errors.NonExhaustiveMatchError
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
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
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
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int = match x with {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
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

  test("Pattern.Tuples.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int = match x with {
        |  case (Color.Red, _) => 1
        |  case (Color.Blu, _) => 2
        |}
      """.stripMargin
    run(input)
  }
  test("Pattern.Tuples.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int = match x with {
        |  case (_,Color.Red ) => 1
        |  case (_, Color.Blu ) => 2
        |}
      """.stripMargin
    run(input)
  }
  test("Pattern.Tuples.03") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int = match x with {
        |  case (Color.Red ,Color.Red ) => 1
        |  case (_, _) => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Tuples.04") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int = match x with {
        |  case (Color.Red() ,_ ) => 1
        |  case (_, Color.Blu) => 2
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Tuples.05") {
    val input =
      """def f(x: (Int8, (Str, Str))): Int = match x with {
        |  case (5i8, ("five", _)) => 5
        |  case (6i8, (_, "six")) => 6
        |  case (7i8, (_,_)) => 7
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Tuples.06") {
    val input =
      """def f(x: (Int, Int, Int, Int, Int)): Int = match x with {
        |  case (1,2,3,4,5) => 1
        |  case (_,2,3,4,5) => 1
        |  case (1,_,3,4,5) => 1
        |  case (1,2,_,4,5) => 1
        |  case (1,2,3,_,5) => 1
        |  case (1,2,3,4,_) => 1
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }
  test("Pattern.Literal.Tuples.07") {
    val input =
      """def f(x: (Int, Int, Int, Int, Int)): Int = match x with {
        |  case (1,2,3,4,5) => 1
        |  case (_,2,3,4,5) => 1
        |  case (1,_,3,4,5) => 1
        |  case (1,2,_,4,5) => 1
        |  case (1,2,3,_,5) => 1
        |  case (1,2,3,4,_) => 1
        |  case (1,2,3,4,_) => 1
        |  case (_,_,_,_,_) => 1
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Pattern.Literal.Lists.01") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(i: Int32, xs: IntList): Int32 = match (i, xs) with {
        |  case (0, Lst(x, _)) => x
        |  case (p, Lst(x, rs)) => x
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Lists.02") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(xs: IntList, ys: IntList): Int32 = match (xs, ys) with {
        |  case (Empty, Empty) => 0
        |  case (Lst(x,xs), Lst(y,ys)) => 1
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.LetMatch01") {
    val input =
      """enum E {
        |  case A(Bool)
        |}
        |
        |def f(e: E): Bool = let E.A(b) = e; b
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch02") {
    val input =
      """enum E {
        |  case A(Bool, Char, Int8)
        |}
        |
        |def f(e: E): Int8 = let E.A(true, 'a', i) = e; i
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.LetMatch03") {
    val input =
      """def f(e: (Int8, Int8)): Int8 = let (a,b) = e; a
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch04") {
    val input =
      """def f(e: (Int8, Int8)): Int8 = let (a,1i8) = e; a
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Deep.01") {
    val input =
      """enum Evil {
        |  case Evil(Evil, Evil),
        |  case Done
        |}
        |
        |def f(x: Evil): Evil = match x with {
        |  case Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, _))))))) => Evil(Done, Done)
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

}
