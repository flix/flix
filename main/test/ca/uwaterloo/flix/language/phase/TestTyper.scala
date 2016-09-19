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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Type.{Bool, Var}
import ca.uwaterloo.flix.language.errors.TypeError
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  /////////////////////////////////////////////////////////////////////////////
  // POSITIVE TEST CASES                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Definitions (Positive)                                                  //
  /////////////////////////////////////////////////////////////////////////////
  test("Definition.Constant02") {
    val input = "def f: Bool = true"
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
  // TODO: Sort

  test("Expression.Bool.True") {
    val input = "def f: Bool = true"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Bool.False") {
    val input = "def f: Bool = false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Int") {
    val input = "def f: Int = 42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Str") {
    val input = "def f: Str = \"foo\""
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tag") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple") {
    val input = "def f: (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref01") {
    val input =
      """def f: Int = 1
        |def g: Int = 2
        |def h: Int = f() + g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref02") {
    val input =
      """namespace A {
        |  def f: Int = 1
        |}
        |namespace B {
        |  def g: Int = 2
        |}
        |
        |def h: Int = A/f() + B/g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Lambda01") {
    val input =
      """
        |def f(g: Int -> Bool): Bool = g(42)
        |
        |def r: Bool = f(x -> x == 21)
        |
      """.stripMargin
    new Flix().addStr(input).solve().get
  }

  test("Expression.Lambda02") {
//    val x = ident("x")
//    val y = ident("y")
//    val z = ident("z")
//    val w = ident("w")
//
//    val rast = ResolvedAst.Expression.Lambda(
//      formals = List(
//        ResolvedAst.FormalArg(x, Type.Unit),
//        ResolvedAst.FormalArg(y, Type.Bool),
//        ResolvedAst.FormalArg(z, Type.Int32),
//        ResolvedAst.FormalArg(w, Type.Str)
//      ),
//      retTpe = Type.Str,
//      body = ResolvedAst.Expression.Var(w, SL)
//      , SL)

    //    val expectedType = Type.Lambda(
    //      args = List(
    //        Type.Unit,
    //        Type.Bool,
    //        Type.Int32,
    //        Type.Str
    //      ), retTpe = Type.Str)
    //val actualType = Typer.Expression.typer(rast, Root).get.tpe
    //assertResult(expectedType)(actualType)
    ???
  }



  test("Expression.Apply01") {
//    val x = ident("x")
//    val rast = ResolvedAst.Expression.Apply(
//      lambda =
//        ResolvedAst.Expression.Lambda(
//          formals = List(ResolvedAst.FormalArg(x, Type.Int32)),
//          retTpe = Type.Unit,
//          body = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit(SL), SL)
//          , SL),
//      args = List(ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int32(42, SL), SL)), SL)

    //val result = Typer.Expression.typer(rast, Root)
    //assertResult(Type.Unit)(result.get.tpe)
    ???
  }

  test("Expression.Apply02") {
//    val x = ident("x")
//    val y = ident("y")
//    val z = ident("z")
//
//    val rast = ResolvedAst.Expression.Apply(
//      lambda =
//        ResolvedAst.Expression.Lambda(
//          formals = List(
//            ResolvedAst.FormalArg(x, Type.Bool),
//            ResolvedAst.FormalArg(y, Type.Int32),
//            ResolvedAst.FormalArg(z, Type.Str)
//          ),
//          retTpe = Type.Int32,
//          body = ResolvedAst.Expression.Var(y, SL)
//          , SL),
//      args = List(
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int32(42, SL), SL),
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
//      ), SL)

    //val result = Typer.Expression.typer(rast, Root)
    //assertResult(Type.Int32)(result.get.tpe)
    ???
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
    val input = "def f: Int = +42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Plus02") {
    val input = "def f(x: Int): Int = +x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus01") {
    val input = "def f: Int = -42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus02") {
    val input = "def f(x: Int): Int = -x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate01") {
    val input = "def f: Int = ~42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate02") {
    val input = "def f(x: Int): Int = ~x"
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
    val input = "def f: Bool = true && false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalAnd02") {
    val input = "def f(x: Bool, y: Bool): Bool = x && y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr01") {
    val input = "def f: Bool = true || false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr02") {
    val input = "def f(x: Bool, y: Bool): Bool = x || y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Implication01") {
    val input = "def f: Bool = true ==> false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Implication02") {
    val input = "def f(x: Bool, y: Bool): Bool = x ==> y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Biconditional01") {
    val input = "def f: Bool = true <==> false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Biconditional02") {
    val input = "def f(x: Bool, y: Bool): Bool = x <==> y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd01") {
    val input = "def f: Int = 1 & 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd02") {
    val input = "def f(x: Int, y: Int): Int = x & y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr01") {
    val input = "def f: Int = 1 | 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr02") {
    val input = "def f(x: Int, y: Int): Int = x | y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor01") {
    val input = "def f: Int = 1 ^ 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor02") {
    val input = "def f(x: Int, y: Int): Int = x ^ y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseLeftShift") {
    val input = "def f(x: Int): Int = x << 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseRightShift") {
    val input = "def f(x: Int): Int = x >> 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else (Positive)                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse01") {
    val input = "def f: Int = if (true) 1 else 2"
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
    val input = "def f: Int = let x = 42 in x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let02") {
    val input = "def f(x: Int): Int = let y = 42 in x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let03") {
    val input = "def f(x: Bool): Int = let x = 42 in x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // LetMatch (Positive)                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.LetMatch01") {
    val input = "def f: Int = let (x, y) = (1, 2) in x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch02") {
    val input = "def f: Int8 = let (x, (y, z, w)) = (true, ('a', 1i8, 2i8)) in z + w"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch03") {
    val input =
      """enum E {
        |  case A(Bool),
        |  case B(Char),
        |  case C(Int)
        |}
        |
        |def f(e: E): Bool = let E.A(b) = e in b
        |def g(e: E): Char = let E.B(c) = e in c
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.LetMatch04") {
    val input =
      """enum E {
        |  case A(Bool, Char, Int8),
        |  case B
        |}
        |
        |def f(e: E): Int8 = let E.A(true, 'a', i) = e in i
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Match (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Match.Wildcard") {
    val input =
      """def f: Int = match true with {
        |  case _ => 42
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal01") {
    val input =
      """def f: Int = match true with {
        |  case true => 42
        |  case false => 21
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal02") {
    val input =
      """def f: Bool = match 1 with {
        |  case 1 => true
        |  case 2 => true
        |  case 3 => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var01") {
    val input =
      """def f: Int = match 42 with {
        |  case x => x
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var02") {
    val input =
      """def f: Int = match (21, 42) with {
        |  case (x, y) => x + y
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Mixed") {
    val input =
      """def f: Bool = match (true, 42, "foo") with {
        |  case (false, 21, "bar") => true
        |  case (x, y, z) => x
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Tuple01") {
    val input =
      """def f: Int = match (true, 42, "foo") with {
        |  case (false, 21, "bar") => 42
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
        |def f: Color = Color.Red
        |def g: Color = Color.Green
        |def h: Color = Color.Blue
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

  /////////////////////////////////////////////////////////////////////////////
  // Tuple (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Tuple01") {
    val input = "def f: (Int, Int, Int) = (1, 2, 3)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple02") {
    val input = "def f(x: Int8, y: Int16, z: Int32, w: Int64): (Int64, Int32, Int16, Int8) = (w, z, y, x)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple03") {
    val input = "def f: (Int, (Int, (Int, Int))) = (1, (2, (3, 4)))"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple04") {
    val input = "def f: (((Int, Int), Int), Int) = (((1, 2), 3), 4)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple05") {
    val input = "def f: (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ascribe (Positive)                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Ascribe01") {
    val input = "def f: Bool = true : Bool"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe02") {
    val input = "def f: Int = 42 : Int"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe03") {
    val input = "def f(x: Int8, y: Int16, z: Int32): (Int32, Int16, Int8) = (z, y, x) : (Int32, Int16, Int8)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("NoSuchLattice01") {
    val input =
      s"""namespace A {
          |  lat A(x: Int, y: Int);
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError.NoSuchLattice])
  }

  test("NoSuchLattice02") {
    val input =
      s"""namespace A {
          |  enum Elm {
          |    case Foo
          |  }
          |
         |  lat A(x: Int, y: Elm);
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError.NoSuchLattice])
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
    val input = "def f: Int32 = 42 : Int32"
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
      """let Int<> = (0, 1, 2, 3, 4);
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Definition.BoundedLattice.TypeError02") {
    val input =
      """|def leq(x: Int, y: Int): Bool = true;
        |def lub(x: Int, y: Int): Int = 42;
        |def glb(x: Int, y: Int): Int = 21;
        |
        |let Int<> = (0, 1, lub, leq, glb);
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Lambda.TypeError") {
//    val x = ident("x")
//    val y = ident("y")
//    val z = ident("z")
//    val w = ident("w")
//
//    val rast = ResolvedAst.Expression.Lambda(
//      formals = List(
//        ResolvedAst.FormalArg(x, Type.Unit),
//        ResolvedAst.FormalArg(y, Type.Bool),
//        ResolvedAst.FormalArg(z, Type.Int32),
//        ResolvedAst.FormalArg(w, Type.Str)
//      ),
//      retTpe = Type.Unit,
//      body = ResolvedAst.Expression.Var(w, SL)
//      , SL)

    //val result = Typer.Expression.typer(rast, Root)
    //assert(result.isFailure)
    ???
  }

  test("Expression.Apply.TypeError.IllegalArgumentType") {
//    val x = ident("x")
//    val y = ident("y")
//    val z = ident("z")
//
//    val rast = ResolvedAst.Expression.Apply(
//      lambda =
//        ResolvedAst.Expression.Lambda(
//          formals = List(
//            ResolvedAst.FormalArg(x, Type.Bool),
//            ResolvedAst.FormalArg(y, Type.Int32),
//            ResolvedAst.FormalArg(z, Type.Str)
//          ),
//          retTpe = Type.Int32,
//          body = ResolvedAst.Expression.Var(y, SL), SL
//        ),
//      args = List(
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL),
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int32(42, SL), SL),
//        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL)
//      ), SL)

    //val result = Typer.Expression.typer(rast, Root)
    //assert(result.isFailure)
    ???
  }

  /////////////////////////////////////////////////////////////////////////////
  // NEGATIVE TEST CASES                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Unary (Negative)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary.LogicalNot.TypeError") {
    val input = "def f: Bool = !42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.Plus.TypeError") {
    val input = "def f: Int = +true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.Minus.TypeError") {
    val input = "def f: Int = -true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.Unary.BitwiseNegate.TypeError") {
    val input = "def f: Int = ~true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else (Negative)                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse.TypeError.NonBooleanCondition") {
    val input = "def f: Int = if (42) 1 else 2"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  test("Expression.IfThenElse.TypeError.MismatchedBranches") {
    val input = "def f: Int = if (true) true else 1234"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[TypeError])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unresolved Symbols                                                      //
  /////////////////////////////////////////////////////////////////////////////

  test("UnresolvedDefinition01") {
    val input = "def f: Int = x"
    val result = new Flix().addStr(input).compile()
    assertError[TypeError.UnresolvedDefinition](result)
  }

  test("UnresolvedDefinition02") {
    val input =
      s"""
         |namespace A {
         |  def f(x: Int, y: Int): Int = x + y + z;
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[TypeError.UnresolvedDefinition](result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Substitutions                                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Substitution.Empty.01") {
    val tpe = Type.Unit
    val subst = Unification.Substitution.empty
    assertResult(Type.Unit)(subst(tpe))
  }

  test("Substitution.Empty.02") {
    val tpe = Type.Bool
    val subst = Unification.Substitution.empty
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Empty.03") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.empty
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Empty.04") {
    val tpe = Type.mkArrow(Type.Bool, Type.Unit)
    val subst = Unification.Substitution.empty
    assertResult(Type.mkArrow(Type.Bool, Type.Unit))(subst(tpe))
  }

  test("Substitution.Singleton.01") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)
    assertResult(Type.Var(1, Kind.Star))(subst(tpe))
  }

  test("Substitution.Singleton.02") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.Bool)(subst(tpe))
  }

  test("Substitution.Singleton.03") {
    val tpe = Type.mkFOpt(Type.Var(1, Kind.Star))
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.mkFOpt(Type.Bool))(subst(tpe))
  }

  test("Substitution.Singleton.04") {
    val tpe = Type.mkFMap(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.mkFMap(Type.Bool, Type.Var(2, Kind.Star)), Type.Var(2, Kind.Star))(subst(tpe))
  }

  test("Substitution.Singleton.05") {
    val tpe = Type.mkFMap(Type.Var(1, Kind.Star), Type.Var(1, Kind.Star))
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    assertResult(Type.mkFMap(Type.Bool, Type.Bool))(subst(tpe))
  }

  test("Substitution.Singleton.06") {
    val tpe = Type.Var(1, Kind.Star)
    val subst = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.Var(2, Kind.Star))(subst(tpe))
  }

  test("Substitution.++.01") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.02") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.Var(2, Kind.Star)
    assertResult(Type.Char)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.03") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst1 ++ subst2) (tpe))
  }

  test("Substitution.++.04") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkArrow(Type.Bool, Type.Char))((subst1 ++ subst2) (tpe))
  }

  test("Substitution.@@.01") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Char)

    val tpe = Type.mkArrow(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assertResult(Type.mkArrow(Type.Bool, Type.Char))((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.02") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Bool)
    val subst2 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Char)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.03") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst2 @@ subst1) (tpe))
  }

  test("Substitution.@@.04") {
    val subst1 = Unification.Substitution.singleton(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    val subst2 = Unification.Substitution.singleton(Type.Var(2, Kind.Star), Type.Var(3, Kind.Star))
    val subst3 = Unification.Substitution.singleton(Type.Var(3, Kind.Star), Type.Bool)

    val tpe = Type.Var(1, Kind.Star)
    assertResult(Type.Bool)((subst3 @@ (subst2 @@ subst1)) (tpe))
  }

  test("Unify.Var.01") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Unit)
    assert(result.isOk)
  }

  test("Unify.Var.02") {
    val result = Unification.unify(Type.Unit, Type.Var(1, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Var.03") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Var(1, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Var.04") {
    val result = Unification.unify(Type.Var(1, Kind.Star), Type.Var(2, Kind.Star))
    assert(result.isOk)
  }

  test("Unify.Unit") {
    val result = Unification.unify(Type.Unit, Type.Unit)
    assert(result.isOk)
  }

  test("Unify.Bool") {
    val result = Unification.unify(Type.Bool, Type.Bool)
    assert(result.isOk)
  }

  test("Unify.Char") {
    val result = Unification.unify(Type.Char, Type.Char)
    assert(result.isOk)
  }

  test("Unify.Float32") {
    val result = Unification.unify(Type.Float32, Type.Float32)
    assert(result.isOk)
  }

  test("Unify.Float64") {
    val result = Unification.unify(Type.Float64, Type.Float64)
    assert(result.isOk)
  }

  test("Unify.Int8") {
    val result = Unification.unify(Type.Int8, Type.Int8)
    assert(result.isOk)
  }

  test("Unify.Int16") {
    val result = Unification.unify(Type.Int16, Type.Int16)
    assert(result.isOk)
  }

  test("Unify.Int32") {
    val result = Unification.unify(Type.Int32, Type.Int32)
    assert(result.isOk)
  }

  test("Unify.Int64") {
    val result = Unification.unify(Type.Int64, Type.Int64)
    assert(result.isOk)
  }

  test("Unify.BigInt") {
    val result = Unification.unify(Type.BigInt, Type.BigInt)
    assert(result.isOk)
  }

  test("Unify.Str") {
    val result = Unification.unify(Type.Str, Type.Str)
    assert(result.isOk)
  }

  test("Unify.Native") {
    val result = Unification.unify(Type.Native, Type.Native)
    assert(result.isOk)
  }

  test("Unify.Arrow") {
    val result = Unification.unify(Type.Arrow(3), Type.Arrow(3))
    assert(result.isOk)
  }

  test("Unify.FTuple") {
    val result = Unification.unify(Type.FTuple(42), Type.FTuple(42))
    assert(result.isOk)
  }

  test("Unify.FOpt.01") {
    val result = Unification.unify(Type.FOpt, Type.FOpt)
    assert(result.isOk)
  }

  test("Unify.FOpt.02") {
    val result = Unification.unify(Type.mkFOpt(Type.Bool), Type.mkFOpt(Type.Bool))
    assert(result.isOk)
  }

  test("Unify.FList.01") {
    val result = Unification.unify(Type.FList, Type.FList)
    assert(result.isOk)
  }

  test("Unify.FList.02") {
    val result = Unification.unify(Type.mkFList(Type.Bool), Type.mkFList(Type.Bool))
    assert(result.isOk)
  }

  test("Unify.FVec.01") {
    val result = Unification.unify(Type.FVec, Type.FVec)
    assert(result.isOk)
  }

  test("Unify.FVec.02") {
    val result = Unification.unify(Type.mkFVec(Type.Bool), Type.mkFVec(Type.Bool))
    assert(result.isOk)
  }

  test("Unify.FSet.01") {
    val result = Unification.unify(Type.FSet, Type.FSet)
    assert(result.isOk)
  }

  test("Unify.FSet.02") {
    val result = Unification.unify(Type.mkFSet(Type.Bool), Type.mkFSet(Type.Bool))
    assert(result.isOk)
  }

  test("Unify.FMap.01") {
    val result = Unification.unify(Type.FMap, Type.FMap)
    assert(result.isOk)
  }

  test("Unify.FMap.02") {
    val result = Unification.unify(Type.mkFMap(Type.Bool, Type.Char), Type.mkFMap(Type.Bool, Type.Char))
    assert(result.isOk)
  }

  test("Unify.Enum.01") {
    val name = Symbol.Resolved.mk("Color")
    val cases = Map.empty[String, Type]
    val result = Unification.unify(Type.Enum(name, cases), Type.Enum(name, cases))
    assert(result.isOk)
  }

  test("Unify.Enum.02") {
    val name = Symbol.Resolved.mk("Color")
    val cases = Map("Red" -> Type.Unit, "Green" -> Type.Unit, "Blue" -> Type.Unit)
    val result = Unification.unify(Type.Enum(name, cases), Type.Enum(name, cases))
    assert(result.isOk)
  }

  test("Unify.Enum.03") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val name = Symbol.Resolved.mk("Color")
    val cases1 = Map(
      "Red" -> Type.Bool,
      "Green" -> Type.Char,
      "Blue" -> Type.Int8
    )
    val cases2 = Map(
      "Red" -> A,
      "Green" -> B,
      "Blue" -> C
    )
    val result = Unification.unify(Type.Enum(name, cases1), Type.Enum(name, cases2)).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
    assertResult(Type.Int8)(result(C))
  }

  test("Unify.01") {
    val tpe1 = Type.Var(1, Kind.Star)
    val tpe2 = Type.Bool
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe1))
  }

  test("Unify.02") {
    val tpe1 = Type.Bool
    val tpe2 = Type.Var(1, Kind.Star)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(tpe2))
  }

  test("Unify.03") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkFOpt(A)
    val tpe2 = Type.mkFOpt(Type.Bool)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
  }

  test("Unify.04") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkFOpt(Type.Bool)
    val tpe2 = Type.mkFOpt(A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
  }

  test("Unify.05") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.06") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Char)(result(A))
  }

  test("Unify.07") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = A
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(tpe1)(result(A))
  }

  test("Unify.08") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = A
    val tpe2 = Type.mkArrow(Type.Bool, Type.Char)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(tpe2)(result(A))
  }

  test("Unify.09") {
    val A = Type.Var(1, Kind.Star)
    val tpe1 = Type.mkArrow(A, Type.Bool)
    val tpe2 = Type.mkArrow(Type.Bool, A)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
  }

  test("Unify.10") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(A, B)
    val tpe2 = Type.mkArrow(Type.Bool, Type.Char)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.11") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(Type.Bool, Type.Char)
    val tpe2 = Type.mkArrow(A, B)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.12") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val tpe1 = Type.mkArrow(A, Type.Char)
    val tpe2 = Type.mkArrow(Type.Bool, B)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(A))
    assertResult(Type.Char)(result(B))
  }

  test("Unify.13") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val tpe1 = Type.mkArrow(A, B)
    val tpe2 = Type.mkArrow(C, Type.Bool)
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.Bool)(result(B))
    assertResult(C)(result(A))
  }

  test("Unify.14") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val tpe1 = Type.mkArrow(Type.mkFOpt(A), B)
    val tpe2 = Type.mkArrow(C, Type.mkFList(Type.Bool))
    val result = Unification.unify(tpe1, tpe2).get
    assertResult(Type.mkFList(Type.Bool))(result(B))
    assertResult(Type.mkFOpt(A))(result(C))
  }

  test("UnifyM.01") {
    val A = Type.Var(1, Kind.Star)
    val B = Type.Var(2, Kind.Star)
    val C = Type.Var(3, Kind.Star)
    val D = Type.Int32
    val Unification.Success(_, subst) = for (
      _ <- Unification.unifyM(A, B);
      _ <- Unification.unifyM(B, C);
      _ <- Unification.unifyM(C, D)
    ) yield null

    assertResult(Type.Int32)(subst(A))
  }

}
