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

  test("Declaration.Relation.01") {
    val input = "rel R(a: Int)"
    run(input)
  }

  test("Declaration.Relation.02") {
    val input = "rel R(a: Char, b: Int, c: Str)"
    run(input)
  }

  test("Declaration.Relation.03") {
    val input = "rel R(a: Int8, b: Int16, c: Int32, d: Int64)"
    run(input)
  }

  test("Lattice.01") {
    val input = "lat L(a: A)"
    expectError[ResolutionError.UndefinedType](new Flix().addStr(input).compile())
  }

  test("Lattice.02") {
    val input = "lat L(a: A, b: B, c: C)"
    expectError[ResolutionError.UndefinedType](new Flix().addStr(input).compile())
  }

  test("Declaration.Index.01") {
    val input =
      """rel R(a: Int)
        |index R({a})
      """.stripMargin
    run(input)
  }

  test("Declaration.Index.02") {
    val input =
      """rel R(a: Char, b: Int)
        |index R({a}, {b})
      """.stripMargin
    run(input)
  }

  test("Declaration.Index.03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |index R({a}, {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}, {a, b, c}, {a, b, c, d})
      """.stripMargin
    run(input)
  }

  test("Declaration.Law.01") {
    val input = "law f(): Bool = true"
    run(input)
  }

  test("Declaration.Law.02") {
    val input = "law f(x: Int): Bool = x % 2 == 0"
    run(input)
  }

  test("Declaration.Law.03") {
    val input = "law f(x: Int, y: Int): Bool = x > y"
    run(input)
  }

  test("Declaration.Fact.01") {
    val input =
      """
        |rel R(a: Int)
        |R(42).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.02") {
    val input =
      """
        |rel R(a: Char, b: Int)
        |R('a', 42).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.03") {
    val input =
      """
        |rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |R(1i8, 2i16, 3i32, 4i64).
      """.stripMargin
    run(input)
  }

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

  test("Expression.UnaryExp.01") {
    val input = "def f(): Int = +1"
    run(input)
  }

  test("Expression.UnaryExp.02") {
    val input = "def f(): Int = -1"
    run(input)
  }

  test("Expression.UnaryExp.03") {
    val input = "def f(): Int = ~~~1"
    run(input)
  }

  test("Expression.UnaryExp.04") {
    val input = "def f(): Bool = !!true"
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

  test("Expression.Switch.01") {
    val input =
      """def f(x: Int): Int = switch {
        |  case true  => 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Switch.02") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Switch.03") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |  case x > 0  => 2
        |  case x == 0 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.01") {
    val input =
      """def f(): Int = match 1 with {
        |  case 2 => 3
        |  case _ => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.02") {
    val input =
      """def f(): Int = match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |  case _ => 5
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.03") {
    val input =
      """def f(): Int = match 1 with {
        |  case 2 => match 3 with {
        |    case 4 => 5
        |    case _ => 5
        |  }
        |  case 6 => 7
        |  case _ => 7
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.04") {
    val input =
      """def f(): Int = match
        |  match 1 with {
        |    case 2 => 3
        |    case _ => 3
        |  } with {
        |    case 4 => 5
        |    case _ => 5
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.01") {
    val input =
      """def f(): Int = 42
        |def g(): Int = f()
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.02") {
    val input =
      """def f(x: Int): Int = x
        |def g(): Int = f(42)
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.03") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g(): Int = f(1, 2)
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.04") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g(): Int = f(1, f(2, 3))
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.05") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g(): Int = f(f(1, 2), f(3, 4))
      """.stripMargin
    run(input)
  }

  test("Expression.Var.01") {
    val input = "def f(x: Int): Int = x"
    run(input)
  }

  test("Expression.Lambda.01") {
    val input = "def f(): Int -> Int = x -> x"
    run(input)
  }

  test("Expression.Lambda.02") {
    val input = "def f(): Int -> Int = (x) -> x"
    run(input)
  }

  test("Expression.Lambda.03") {
    val input = "def f(): (Bool, Char) -> Int = (x, y) -> 42"
    run(input)
  }

  test("Expression.Lambda.04") {
    val input = "def f(): (Bool, Char, Int) -> Int = (x, y, z) -> 42"
    run(input)
  }

  test("Expression.Lambda.05") {
    val input = "def f(): (Int8, Int16, Int32, Int64) -> Int32 = (x, y, z, w) -> z"
    run(input)
  }

  test("Expression.Lambda.06") {
    val input = "def f(): Int -> (Bool, Char) = x -> (true, 'a')"
    run(input)
  }

  test("Expression.Lambda.07") {
    val input = "def f(): Int -> (Bool, Char, Int) = x -> (true, 'a', 42)"
    run(input)
  }

  test("Expression.Lambda.08") {
    val input = "def f(): (Bool, Char) -> (Char, Bool) = (x, y) -> (y, x)"
    run(input)
  }

  test("Expression.Lambda.09") {
    val input = "def f(): (Bool, Char, Int) -> (Int, Char, Bool) = (x, y, z) -> (z, y, x)"
    run(input)
  }

  test("Expression.Lambda.10") {
    val input = "def f(): ((Bool, Char), Int) -> (Bool, Char) = (x, y) -> x"
    run(input)
  }

  test("Expression.Lambda.11") {
    val input = "def f(): (Bool, (Char, Int)) -> (Char, Int) = (x, y) -> y"
    run(input)
  }

  test("Expression.Lambda.12") {
    val input = "def f(): (Int, Int) -> ((Int, Int), (Int, Int)) = (x, y) -> ((x, y), (y, x))"
    run(input)
  }

  test("Expression.Lambda.13") {
    val input = "def f(): Bool -> Char -> Int = x -> (y -> 42)"
    run(input)
  }

  test("Expression.Lambda.14") {
    val input = "def f(): (Bool, Bool) -> Char -> Int = (x1, x2) -> (y -> 42)"
    run(input)
  }

  test("Expression.Lambda.15") {
    val input = "def f(): Bool -> (Char, Char) -> Int = x -> ((y1, y2) -> 42)"
    run(input)
  }

  test("Expression.Lambda.16") {
    val input = "def f(): Bool -> Char -> (Int, Int) = x -> (y -> (21, 42))"
    run(input)
  }

  test("Expression.Lambda.17") {
    val input = "def f(): (Bool, Bool) -> (Char, Char) -> (Int, Int) = (x1, x2) -> ((y1, y2) -> (21, 42))"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
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
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Float32.01") {
    val input =
      """def f(x: Float32): Int = match x with {
        |  case 1.0f32 => 1
        |  case 2.0f32 => 2
        |  case 3.0f32 => 3
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Float64.01") {
    val input =
      """def f(x: Float64): Int = match x with {
        |  case 1.0f64 => 1
        |  case 2.0f64 => 2
        |  case 3.0f64 => 3
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int8.01") {
    val input =
      """def f(x: Int8): Int = match x with {
        |  case 1i8 => 1
        |  case 2i8 => 2
        |  case 3i8 => 3
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int16.01") {
    val input =
      """def f(x: Int16): Int = match x with {
        |  case 1i16 => 1
        |  case 2i16 => 2
        |  case 3i16 => 3
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
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int = match x with {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |  case _ => 4
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |  case _ => 4
        |}
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

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Unit.01") {
    val input = "def f(): Unit = ()"
    run(input)
  }

  test("Type.Bool.01") {
    val input = "def f(): Bool = true"
    run(input)
  }

  test("Type.Bool.02") {
    val input = "def f(): Bool = false"
    run(input)
  }

  test("Type.Char.01") {
    val input = "def f(): Char = 'a'"
    run(input)
  }

  test("Type.Float32.01") {
    val input = "def f(): Float32 = 0.0f32"
    run(input)
  }

  test("Type.Float64.01") {
    val input = "def f(): Float64 = 0.0f64"
    run(input)
  }

  test("Type.Int8.01") {
    val input = "def f(): Int8 = 0i8"
    run(input)
  }

  test("Type.Int16.01") {
    val input = "def f(): Int16 = 0i16"
    run(input)
  }

  test("Type.Int32.01") {
    val input = "def f(): Int32 = 0i32"
    run(input)
  }

  test("Type.Int64.01") {
    val input = "def f(): Int64 = 0i64"
    run(input)
  }

  test("Type.BigInt.01") {
    val input = "def f(): BigInt = 0ii"
    run(input)
  }

  test("Type.Str.01") {
    val input = "def f(): Str = \"foobar\""
    run(input)
  }

  test("Type.Enum.01") {
    val input =
      """enum Color {
        |  case Red
        |}
        |
        |def f(): Color = Color.Red
      """.stripMargin
    run(input)
  }

  test("Type.Tuple.01") {
    val input = "def f(): (Int, Int) = (1, 2)"
    run(input)
  }

  test("Type.Tuple.02") {
    val input = "def f(): (Unit, Bool, Char, Int) = ((), true, 'a', 42)"
    run(input)
  }

  test("Type.Lambda.01") {
    val input = "def f(): Bool -> Int = x -> 42"
    run(input)
  }

  test("Type.Lambda.02") {
    val input = "def f(): (Bool, Char, Int) -> Str = (x,y, z) -> \"a\""
    run(input)
  }

  test("Type.Lambda.03") {
    val input = "def f(): Str -> (Bool, Char, Int) = x -> (true, 'a', 42)"
    run(input)
  }

  test("Type.List.01") {
    val input = "def f(): List[Int] = Nil"
    run(input, core = false)
  }

  test("Type.Set.01") {
    val input = "def f(): Set[Int] = #{}"
    run(input, core = false)
  }

  test("Type.Map.01") {
    val input = "def f(): Map[Int, Int] = @{}"
    run(input, core = false)
  }

  test("Type.Infix.01") {
    val input = "def f(): Char `Map` Int = @{}"
    run(input, core = false)
  }

  test("Type.Infix.02") {
    val input = "def f(): Int `Map` Str = @{}"
    run(input, core = false)
  }

}
