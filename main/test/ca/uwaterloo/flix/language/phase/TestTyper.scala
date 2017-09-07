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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.{ResolutionError, TypeError}
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  /////////////////////////////////////////////////////////////////////////////
  // POSITIVE TEST CASES                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Definitions (Positive)                                                  //
  /////////////////////////////////////////////////////////////////////////////
  test("Definition.Constant02") {
    val input = "def f(): Bool = true"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Constant03") {
    val input = "def f(x: Bool): Bool = x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Constant04") {
    val input = "def f(x: Int): Int = x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Relation01") {
    val input = "rel R(x: Bool)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Relation02") {
    val input = "rel R(x: Bool, y: Int, z: Str)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Constraints                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Constraint.Fact01") {
    val input =
      """rel R(x: Bool, y: Int, z: Str)
        |R(true, 42, "foo").
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Constraint.Rule01") {
    val input =
      """rel R(x: Int, y: Int)
        |R(x, y) :- R(y, x).
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Bool.True") {
    val input = "def f(): Bool = true"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Bool.False") {
    val input = "def f(): Bool = false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Int") {
    val input = "def f(): Int = 42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Str") {
    val input = "def f(): Str = \"foo\""
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(): Color = Red
        |def g(): Color = Green
        |def h(): Color = Blue
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(): Color = Color.Red
        |def g(): Color = Color.Green
        |def h(): Color = Color.Blue
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag.03") {
    val input =
      """enum A {
        |  case X
        |}
        |
        |enum B {
        |  case X
        |}
        |
        |enum C {
        |  case X
        |}
        |
        |def f(): A = A.X
        |def g(): B = B.X
        |def h(): C = C.X
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple") {
    val input = "def f(): (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref01") {
    val input =
      """def f(): Int = 1
        |def g(): Int = 2
        |def h(): Int = f() + g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref02") {
    val input =
      """namespace A {
        |  def f(): Int = 1
        |}
        |namespace B {
        |  def g(): Int = 2
        |}
        |
        |def h(): Int = A.f() + B.g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Lambda01") {
    val input =
      """
        |def f(g: Int -> Bool): Bool = g(42)
        |
        |def r(): Bool = f(x -> x == 21)
        |
      """.stripMargin
    new Flix().addStr(input).solve().get
  }

  test("Expression.Lambda02") {
    val input =
      """
        |def f(g: (Bool, Char, Int) -> Bool): Bool = g(true, 'a', 42)
        |
        |def r(): Bool = f((x, y, z) -> z == 21)
        |
      """.stripMargin
    new Flix().addStr(input).solve().get
  }

  test("Expression.Lambda03") {
    val input =
      """
        |def f(w: Int): Int -> Bool = x -> x == w
        |
        |def r(): Int -> Bool = f(42)
        |
      """.stripMargin
    new Flix().addStr(input).solve().get
  }

  test("Expression.Lambda04") {
    val input =
      """
        |def f(w: Int): (Bool, Int, Char) -> Bool = (x, y, z) -> y == w
        |
        |def r(): (Bool, Int, Char) -> Bool = f(42)
        |
      """.stripMargin
    new Flix().addStr(input).solve().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unary (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary.Not") {
    val input = "def f(x: Bool): Bool = !x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.NotNot") {
    val input = "def f(x: Bool): Bool = !!x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Plus01") {
    val input = "def f(): Int = +42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Plus02") {
    val input = "def f(x: Int): Int = +x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus01") {
    val input = "def f(): Int = -42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus02") {
    val input = "def f(x: Int): Int = -x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate01") {
    val input = "def f(): Int = ~~~42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate02") {
    val input = "def f(x: Int): Int = ~~~x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary (Positive)                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Binary.Plus") {
    val input = "def f(x: Int, y: Int): Int = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Minus") {
    val input = "def f(x: Int, y: Int): Int = x - y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Times") {
    val input = "def f(x: Int, y: Int): Int = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Divide") {
    val input = "def f(x: Int, y: Int): Int = x / y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Modulo") {
    val input = "def f(x: Int, y: Int): Int = x % y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Less") {
    val input = "def f(x: Int, y: Int): Bool = x < y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LessEqual") {
    val input = "def f(x: Int, y: Int): Bool = x <= y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Greater") {
    val input = "def f(x: Int, y: Int): Bool = x > y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.GreaterEqual") {
    val input = "def f(x: Int, y: Int): Bool = x >= y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal01") {
    val input = "def f(x: Bool, y: Bool): Bool = x == y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal02") {
    val input = "def f(x: Int, y: Int): Bool = x == y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal03") {
    val input = "def f(x: Str, y: Str): Bool = x == y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal04") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(x: Color, y: Color): Bool = x == Color.Red
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual01") {
    val input = "def f(x: Bool, y: Bool): Bool = x != y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual02") {
    val input = "def f(x: Int, y: Int): Bool = x != y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual03") {
    val input = "def f(x: Str, y: Str): Bool = x != y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual04") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(x: Color, y: Color): Bool = x != Color.Red
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalAnd01") {
    val input = "def f(): Bool = true && false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalAnd02") {
    val input = "def f(x: Bool, y: Bool): Bool = x && y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr01") {
    val input = "def f(): Bool = true || false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr02") {
    val input = "def f(x: Bool, y: Bool): Bool = x || y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd01") {
    val input = "def f(): Int = 1 &&& 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd02") {
    val input = "def f(x: Int, y: Int): Int = x &&& y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr01") {
    val input = "def f(): Int = 1 ||| 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr02") {
    val input = "def f(x: Int, y: Int): Int = x ||| y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor01") {
    val input = "def f(): Int = 1 ^^^ 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor02") {
    val input = "def f(x: Int, y: Int): Int = x ^^^ y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseLeftShift") {
    val input = "def f(x: Int): Int = x <<< 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseRightShift") {
    val input = "def f(x: Int): Int = x >>> 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else (Positive)                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse01") {
    val input = "def f(): Int = if (true) 1 else 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.IfThenElse02") {
    val input = "def f(x: Bool, y: Int, z: Int): Int = if (x) y else z"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.IfThenElse03") {
    val input = "def f(x: Int, y: Int, z: Int): Int = if (x != y) y else z"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Let (Positive)                                                          //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Let01") {
    val input = "def f(): Int = let x = 42; x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let02") {
    val input = "def f(x: Int): Int = let y = 42; x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let03") {
    val input = "def f(x: Bool): Int = let x = 42; x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // LetMatch (Positive)                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.LetMatch01") {
    val input = "def f(): Int = let (x, y) = (1, 2); x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch02") {
    val input = "def f(): Int8 = let (x, (y, z, w)) = (true, ('a', 1i8, 2i8)); z + w"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch03") {
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

  test("Expression.LetMatch04") {
    val input =
      """enum E {
        |  case A(Bool, Char, Int8)
        |}
        |
        |def f(e: E): Int8 = let E.A(i, j, k) = e; k
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Match (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Match.Wildcard") {
    val input =
      """def f(): Int = match true with {
        |  case _ => 42
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal01") {
    val input =
      """def f(): Int = match true with {
        |  case true => 42
        |  case false => 21
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal02") {
    val input =
      """def f(): Bool = match 1 with {
        |  case 1 => true
        |  case 2 => true
        |  case 3 => true
        |  case _ => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var01") {
    val input =
      """def f(): Int = match 42 with {
        |  case x => x
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var02") {
    val input =
      """def f(): Int = match (21, 42) with {
        |  case (x, y) => x + y
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Mixed") {
    val input =
      """def f(): Bool = match (true, 42, "foo") with {
        |  case (false, 21, "bar") => true
        |  case (x, y, z) => x
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Tuple01") {
    val input =
      """def f(): Int = match (true, 42, "foo") with {
        |  case (false, 21, "bar") => 42
        |  case _ => 42
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Tag01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Color.Red => 1
        |  case Color.Green => 2
        |  case Color.Blue => 3
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Guard01") {
    val input =
      """enum Color {
        |  case Red(Int),
        |  case Green(Int),
        |  case Blue(Int)
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Color.Red(x)   if x == 0  => 1
        |  case Color.Green(x) if x >= 0  => 2
        |  case Color.Blue(x)  if x <= 0  => 3
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Guard02") {
    val input =
      """enum Color {
        |  case Red(Int, Int, Int),
        |  case Green(Int, Int, Int),
        |  case Blue(Int, Int, Int)
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Color.Red(x, y, z)   if x + y + z == 0  => 1
        |  case Color.Green(x, y, z) if x + y + z >= 0  => 2
        |  case Color.Blue(x, y, z)  if x + y + z <= 0  => 3
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tag (Positive)                                                          //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Tag01") {
    val input =
      """enum Color {
        |  case Red
        |}
        |
        |def f(c: Color): Color = c
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f(): Color = Color.Red
        |def g(): Color = Color.Green
        |def h(): Color = Color.Blue
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag03") {
    val input =
      """enum Color {
        |  case Red(Bool),
        |  case Green(Int),
        |  case Blue(Str)
        |}
        |
        |def f(x: Bool): Color = Color.Red(x)
        |def g(x: Int): Color = Color.Green(x)
        |def h(x: Str): Color = Color.Blue(x)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag04") {
    val input =
      """enum Intensity {
        |  case Bright,
        |  case Dark
        |}
        |
        |enum Color {
        |  case Red(Intensity),
        |  case Green(Intensity),
        |  case Blue(Intensity)
        |}
        |
        |def f(x: Bool): Color = Color.Green(Intensity.Dark)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag05") {
    val input =
      """enum Foo[a] {
        |  case Foo(a)
        |}
        |
        |def f(x: Int): Foo[Int] = Foo.Foo(x)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag06") {
    val input =
      """enum Foo[a, b, c] {
        |  case Foo(a, b, c)
        |}
        |
        |def f(x: Bool, y: Char, z: Int): Foo[Bool, Char, Int] = Foo.Foo(x, y, z)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.List.01") {
    val input =
      """
         def f(): List[Int] = Nil
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.List.02") {
    val input =
      """
        |def f(): List[Int] = {
        |  let x = Nil;
        |  let y = Nil;
        |    x
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.List.03") {
    val input =
      """
        |def f(xs: List[Int], ys: List[Int]): Int = match (xs, ys) with {
        |  case (Nil, Nil) => 1
        |  case (Nil, _)   => 2
        |  case (_, Nil)   => 3
        |  case _          => 4
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.List.04") {
    val input =
      """
        |def f(xs: List[Int8], ys: List[Int16], zs: List[Int32], ws: List[Int64]): Int8 = match (xs, ys, zs, ws) with {
        |  case (x, Nil, Nil, Nil) => 42i8
        |  case _                  => 21i8
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.List.05") {
    val input =
      """
        |def f(xs: List[Int], ys: List[Int]): Int = match (xs, ys) with {
        |  case (42 :: xss, y :: yss) => f(xss, y :: yss)
        |  case _ => 42
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tuple (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Tuple01") {
    val input = "def f(): (Int, Int, Int) = (1, 2, 3)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple02") {
    val input = "def f(x: Int8, y: Int16, z: Int32, w: Int64): (Int64, Int32, Int16, Int8) = (w, z, y, x)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple03") {
    val input = "def f(): (Int, (Int, (Int, Int))) = (1, (2, (3, 4)))"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple04") {
    val input = "def f(): (((Int, Int), Int), Int) = (((1, 2), 3), 4)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple05") {
    val input = "def f(): (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // UserError (Positive)                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.UserError.01") {
    val input = "def f(): Unit = ???"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.UserError.02") {
    val input = "def f(): Int = ???"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ascribe (Positive)                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Ascribe01") {
    val input = "def f(): Bool = true : Bool"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe02") {
    val input = "def f(): Int = 42 : Int"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe03") {
    val input = "def f(x: Int8, y: Int16, z: Int32): (Int32, Int16, Int8) = (z, y, x) : (Int32, Int16, Int8)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Type.Int8.Plus") {
    val input = "def f(x: Int8, y: Int8): Int8 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int8.Times") {
    val input = "def f(x: Int8, y: Int8): Int8 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int16.Plus") {
    val input = "def f(x: Int16, y: Int16): Int16 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int16.Times") {
    val input = "def f(x: Int16, y: Int16): Int16 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Ascribe") {
    val input = "def f(): Int32 = 42 : Int32"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Plus") {
    val input = "def f(x: Int32, y: Int32): Int32 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Times") {
    val input = "def f(x: Int32, y: Int32): Int32 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int64.Plus") {
    val input = "def f(x: Int64, y: Int64): Int64 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int64.Times") {
    val input = "def f(x: Int64, y: Int64): Int64 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.BoundedLattice.TypeError01") {
    val input =
      """let Int<> = (0, 1, 2, 3, 4, 5)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Definition.BoundedLattice.TypeError02") {
    val input =
      """def leq(x: Int, y: Int): Bool = true
        |def lub(x: Int, y: Int): Int = 42
        |def glb(x: Int, y: Int): Int = 21
        |
        |let Int<> = (0, 1, 2, lub, leq, glb)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  /////////////////////////////////////////////////////////////////////////////
  // NEGATIVE TEST CASES                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Unary (Negative)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary.LogicalNot.TypeError") {
    val input = "def f(): Bool = !42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.Plus.TypeError") {
    val input = "def f(): Int = +true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.Minus.TypeError") {
    val input = "def f(): Int = -true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.BitwiseNegate.TypeError") {
    val input = "def f(): Int = ~~~true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else (Negative)                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse.TypeError.NonBooleanCondition") {
    val input = "def f(): Int = if (42) 1 else 2"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.IfThenElse.TypeError.MismatchedBranches") {
    val input = "def f(): Int = if (true) true else 1234"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unresolved Symbols                                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Move into other test suite
  test("UnresolvedDefinition01") {
    val input = "def f(): Int = x"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO: Move into other test suite
  test("UnresolvedDefinition02") {
    val input =
      s"""
         |namespace A {
         |  def f(x: Int, y: Int): Int = x + y + z
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO: Move into other test suite
  test("UnresolvedTable01") {
    val input = "VarPointsTo(1, 2)."
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

  // TODO: Move into other test suite
  test("UnresolvedTable02") {
    val input =
      s"""namespace A {
          |  VarPointsTo(1, 2).
          |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

  // TODO: Move into other test suite
  test("UnresolvedTable03") {
    val input = "index AddrOf({foo, bar})"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

}
