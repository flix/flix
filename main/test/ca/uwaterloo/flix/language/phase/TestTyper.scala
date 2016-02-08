package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import org.scalatest.FunSuite

class TestTyper extends FunSuite {

  // TODO: Consider using real syntax?
  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

  @deprecated
  val SL = SourceLocation.Unknown
  @deprecated
  val Root = ResolvedAst.Root(Map.empty, List.empty, Map.empty, Map.empty, Map.empty, Map.empty, List.empty, List.empty, Map.empty, new Time(0, 0, 0, 0, 0))
  @deprecated
  val Ident = ident("x")
  @deprecated
  val RName = Name.Resolved.mk(List("foo", "bar"))

  /////////////////////////////////////////////////////////////////////////////
  // POSITIVE TEST CASES                                                     //
  /////////////////////////////////////////////////////////////////////////////

  /////////////////////////////////////////////////////////////////////////////
  // Definitions (Positive)                                                  //
  /////////////////////////////////////////////////////////////////////////////
  ignore("Definition.Constant01") {
    val input = "fn f(): Unit = ()"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Constant02") {
    val input = "fn f(): Bool = true"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Constant03") {
    val input = "fn f(x: Bool): Bool = x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Definition.Constant04") {
    val input = "fn f(x: Int): Int = x"
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

  ignore("Expression.Unit") {
    val input = "fn f(): Unit = ()"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Bool.True") {
    val input = "fn f(): Bool = true"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Bool.False") {
    val input = "fn f(): Bool = false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Int") {
    val input = "fn f(): Int = 42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Str") {
    val input = "fn f(): Str = \"foo\""
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
        |fn f(): Color = Color.Red
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple") {
    val input = "fn f(): (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref01") {
    val input =
      """fn f(): Int = 1
        |fn g(): Int = 2
        |fn h(): Int = f() + g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ref02") {
    val input =
      """namespace A {
        |  fn f(): Int = 1
        |}
        |namespace B {
        |  fn g(): Int = 2
        |}
        |
        |fn h(): Int = A::f() + B::g()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Lambda01") {
    val x = ident("x")

    val rast = ResolvedAst.Expression.Lambda(
      Ast.Annotations(List.empty),
      formals = List(ResolvedAst.FormalArg(x, Type.Int32)),
      retTpe = Type.Unit,
      body = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit(SL), SL)
      , SL)

    val expectedType = Type.Lambda(List(Type.Int32), Type.Unit)
    val actualType = Typer.Expression.typer(rast, Root).get.tpe
    assertResult(expectedType)(actualType)
  }

  test("Expression.Lambda02") {
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")
    val w = ident("w")

    val rast = ResolvedAst.Expression.Lambda(
      Ast.Annotations(List.empty),
      formals = List(
        ResolvedAst.FormalArg(x, Type.Unit),
        ResolvedAst.FormalArg(y, Type.Bool),
        ResolvedAst.FormalArg(z, Type.Int32),
        ResolvedAst.FormalArg(w, Type.Str)
      ),
      retTpe = Type.Str,
      body = ResolvedAst.Expression.Var(w, SL)
      , SL)

    val expectedType = Type.Lambda(
      args = List(
        Type.Unit,
        Type.Bool,
        Type.Int32,
        Type.Str
      ), retTpe = Type.Str)
    val actualType = Typer.Expression.typer(rast, Root).get.tpe
    assertResult(expectedType)(actualType)
  }



  test("Expression.Apply01") {
    val x = ident("x")
    val rast = ResolvedAst.Expression.Apply(
      lambda =
        ResolvedAst.Expression.Lambda(
          Ast.Annotations(List.empty),
          formals = List(ResolvedAst.FormalArg(x, Type.Int32)),
          retTpe = Type.Unit,
          body = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit(SL), SL)
          , SL),
      args = List(ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL)), SL)

    val result = Typer.Expression.typer(rast, Root)
    assertResult(Type.Unit)(result.get.tpe)
  }

  test("Expression.Apply02") {
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")

    val rast = ResolvedAst.Expression.Apply(
      lambda =
        ResolvedAst.Expression.Lambda(
          Ast.Annotations(List.empty),
          formals = List(
            ResolvedAst.FormalArg(x, Type.Bool),
            ResolvedAst.FormalArg(y, Type.Int32),
            ResolvedAst.FormalArg(z, Type.Str)
          ),
          retTpe = Type.Int32,
          body = ResolvedAst.Expression.Var(y, SL)
          , SL),
      args = List(
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL),
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
      ), SL)

    val result = Typer.Expression.typer(rast, Root)
    assertResult(Type.Int32)(result.get.tpe)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Unary (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary.Not") {
    val input = "fn f(x: Bool): Bool = !x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.NotNot") {
    val input = "fn f(x: Bool): Bool = !!x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Plus01") {
    val input = "fn f(): Int = +42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Plus02") {
    val input = "fn f(x: Int): Int = +x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus01") {
    val input = "fn f(): Int = -42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.Minus02") {
    val input = "fn f(x: Int): Int = -x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate01") {
    val input = "fn f(): Int = ~42"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Unary.BitwiseNegate02") {
    val input = "fn f(x: Int): Int = ~x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Binary (Positive)                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Binary.Plus") {
    val input = "fn f(x: Int, y: Int): Int = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Minus") {
    val input = "fn f(x: Int, y: Int): Int = x - y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Times") {
    val input = "fn f(x: Int, y: Int): Int = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Divide") {
    val input = "fn f(x: Int, y: Int): Int = x / y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Modulo") {
    val input = "fn f(x: Int, y: Int): Int = x % y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Less") {
    val input = "fn f(x: Int, y: Int): Bool = x < y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LessEqual") {
    val input = "fn f(x: Int, y: Int): Bool = x <= y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Greater") {
    val input = "fn f(x: Int, y: Int): Bool = x > y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.GreaterEqual") {
    val input = "fn f(x: Int, y: Int): Bool = x >= y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal01") {
    val input = "fn f(x: Bool, y: Bool): Bool = x == y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal02") {
    val input = "fn f(x: Int, y: Int): Bool = x == y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Equal03") {
    val input = "fn f(x: Str, y: Str): Bool = x == y"
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
        |fn f(x: Color, y: Color): Bool = x == Color.Red
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual01") {
    val input = "fn f(x: Bool, y: Bool): Bool = x != y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual02") {
    val input = "fn f(x: Int, y: Int): Bool = x != y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.NotEqual03") {
    val input = "fn f(x: Str, y: Str): Bool = x != y"
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
        |fn f(x: Color, y: Color): Bool = x != Color.Red
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalAnd01") {
    val input = "fn f(): Bool = true && false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalAnd02") {
    val input = "fn f(x: Bool, y: Bool): Bool = x && y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr01") {
    val input = "fn f(): Bool = true || false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.LogicalOr02") {
    val input = "fn f(x: Bool, y: Bool): Bool = x || y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Implication01") {
    val input = "fn f(): Bool = true ==> false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Implication02") {
    val input = "fn f(x: Bool, y: Bool): Bool = x ==> y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Biconditional01") {
    val input = "fn f(): Bool = true <==> false"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.Biconditional02") {
    val input = "fn f(x: Bool, y: Bool): Bool = x <==> y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd01") {
    val input = "fn f(): Int = 1 & 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseAnd02") {
    val input = "fn f(x: Int, y: Int): Int = x & y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr01") {
    val input = "fn f(): Int = 1 | 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseOr02") {
    val input = "fn f(x: Int, y: Int): Int = x | y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor01") {
    val input = "fn f(): Int = 1 ^ 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseXor02") {
    val input = "fn f(x: Int, y: Int): Int = x ^ y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseLeftShift") {
    val input = "fn f(x: Int): Int = x << 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Binary.BitwiseRightShift") {
    val input = "fn f(x: Int): Int = x >> 1"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else (Positive)                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse01") {
    val input = "fn f(): Int = if (true) 1 else 2"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.IfThenElse02") {
    val input = "fn f(x: Bool, y: Int, z: Int): Int = if (x) y else z"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.IfThenElse03") {
    val input = "fn f(x: Int, y: Int, z: Int): Int = if (x != y) y else z"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Let (Positive)                                                          //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Let01") {
    val input = "fn f(): Int = let x = 42 in x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let02") {
    val input = "fn f(x: Int): Int = let y = 42 in x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Let03") {
    val input = "fn f(x: Bool): Int = let x = 42 in x"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Match (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Match.Wildcard") {
    val input =
      """fn f(): Int = match true with {
        |  case _ => 42
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal01") {
    val input =
      """fn f(): Int = match true with {
        |  case true => 42
        |  case false => 21
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Literal02") {
    val input =
      """fn f(): Bool = match 1 with {
        |  case 1 => true
        |  case 2 => true
        |  case 3 => true
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var01") {
    val input =
      """fn f(): Int = match 42 with {
        |  case x => x
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Match.Var02") {
    val input =
      """fn f(): Int = match (21, 42) with {
        |  case (x, y) => x + y
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }



  test("Expression.Match.Mixed") {
    val input =
      """fn f(): Bool = match (true, 42, "foo") with {
        |  case (false, 21, "bar") => true
        |  case (x, y, z) => x
        |}
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }


  test("Pattern.Tag01") {
    val tagName = ident("Qux")
    val x = ident("x")
    val rast = ResolvedAst.Pattern.Tag(RName, tagName, ResolvedAst.Pattern.Var(x, SL), SL)
    val tpe = Type.Enum(Name.Resolved.mk("foo"), Map("Qux" -> Type.Tag(RName, tagName, Type.Unit)))
    val result = Typer.Pattern.typer(rast, tpe, Root)
    assertResult(Type.Unit)(result.get.freeVars(x.name))
  }

  test("Pattern.Tuple01") {
    val rast = ResolvedAst.Pattern.Tuple(List(
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Unit(SL), SL),
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Bool(true, SL), SL)
    ), SL)
    val tpe = Type.Tuple(List(
      Type.Unit,
      Type.Bool
    ))
    val result = Typer.Pattern.typer(rast, tpe, Root)
    assertResult(tpe)(result.get.tpe)
  }

  test("Pattern.Tuple02") {
    val rast = ResolvedAst.Pattern.Tuple(List(
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Unit(SL), SL),
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int(42, SL), SL),
      ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
    ), SL)

    val tpe = Type.Tuple(List(
      Type.Unit,
      Type.Bool,
      Type.Int32,
      Type.Str
    ))
    val result = Typer.Pattern.typer(rast, tpe, Root)
    assertResult(tpe)(result.get.tpe)
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
        |fn f(c: Color): Color = c
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
        |fn f(): Color = Color.Red
        |fn g(): Color = Color.Green
        |fn h(): Color = Color.Blue
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
        |fn f(x: Bool): Color = Color.Red(x)
        |fn g(x: Int): Color = Color.Green(x)
        |fn h(x: Str): Color = Color.Blue(x)
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
        |fn f(x: Bool): Color = Color.Green(Intensity.Dark)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tuple (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Tuple01") {
    val input = "fn f(): (Int, Int, Int) = (1, 2, 3)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple02") {
    val input = "fn f(x: Int8, y: Int16, z: Int32, w: Int64): (Int64, Int32, Int16, Int8) = (w, z, y, x)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple03") {
    val input = "fn f(): (Int, (Int, (Int, Int))) = (1, (2, (3, 4)))"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple04") {
    val input = "fn f(): (((Int, Int), Int), Int) = (((1, 2), 3), 4)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Tuple05") {
    val input = "fn f(): (Bool, Int, Str) = (true, 42, \"foo\")"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ascribe (Positive)                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Ascribe01") {
    val input = "fn f(): Bool = true : Bool"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe02") {
    val input = "fn f(): Int = 42 : Int"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Ascribe03") {
    val input = "fn f(x: Int8, y: Int16, z: Int32): (Int32, Int16, Int8) = (z, y, x) : (Int32, Int16, Int8)"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Error (Positive)                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Error01") {
    val input = "fn f(): Bool = ??? : Bool"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Expression.Error02") {
    val input = "fn f(): Int = ??? : Int"
    val result = new Flix().addStr(input).compile()
    result.get
  }



  test("Expression.Unary.NonBooleanValue") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.LogicalNot, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL), SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Unary.NonIntegerValue01") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.Plus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL), SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Unary.NonIntegerValue02") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.Minus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL), SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.IfThenElse.NonBooleanCondition") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(1, SL), SL),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(2, SL), SL),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(3, SL), SL)
      , SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.IfThenElse.ThenElseMismatch") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(2, SL), SL),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
      , SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Let.TypeError") {
    val rast = ResolvedAst.Expression.Let(
      Ident,
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL),
      ResolvedAst.Expression.Unary(UnaryOperator.LogicalNot, ResolvedAst.Expression.Var(Ident, SL), SL)
      , SL)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }


  test("Expression.Match.TypeError") {
    val rast = ResolvedAst.Expression.Match(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
      List(
        ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Int(42, SL), SL) -> ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL),
        ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Str("foo", SL), SL) -> ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
      ), SL
    )

    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Tag.TypeError") {
    val enumName = Name.Resolved.mk(List("Foo", "Bar"))
    val tagName = ident("A")
    val rast = ResolvedAst.Expression.Tag(enumName, tagName, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL), SL)

    val root = Root.copy(enums = Map(
      enumName -> ResolvedAst.Definition.Enum(enumName, Map(
        "A" -> Type.Tag(enumName, tagName, Type.Unit),
        "B" -> Type.Tag(enumName, tagName, Type.Bool),
        "C" -> Type.Tag(enumName, tagName, Type.Int32),
        "D" -> Type.Tag(enumName, tagName, Type.Str)
      ), SL)
    ))

    val result = Typer.Expression.typer(rast, root)
    assert(result.isFailure)
  }



  /////////////////////////////////////////////////////////////////////////////
  // Predicates & Terms                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("Predicate.Head01") {
    val rname = Name.Resolved.mk(List("foo", "bar"))
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")
    val w = ident("w")

    val root = Root.copy(collections = Map(
      rname -> ResolvedAst.Collection.Relation(rname, List(
        ResolvedAst.Attribute(x, Type.Unit),
        ResolvedAst.Attribute(y, Type.Bool),
        ResolvedAst.Attribute(z, Type.Int32),
        ResolvedAst.Attribute(w, Type.Str)
      ), SL)
    ))

    val rast =
      ResolvedAst.Predicate.Head.Relation(rname, List(
        ResolvedAst.Term.Head.Lit(ResolvedAst.Literal.Unit(SL), SL),
        ResolvedAst.Term.Head.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
        ResolvedAst.Term.Head.Lit(ResolvedAst.Literal.Int(42, SL), SL),
        ResolvedAst.Term.Head.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
      ), SL)

    val expectedType = Type.Predicate(List(
      Type.Unit, Type.Bool, Type.Int32, Type.Str
    ))
    val actualType = Typer.Predicate.Head.typer(rast, root).get.tpe
    assertResult(expectedType)(actualType)
  }

  test("Predicate.Head02") {
    val rname = Name.Resolved.mk(List("foo", "bar"))
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")
    val w = ident("w")

    // NB: Somewhat misleading we use the same identifiers for both columns and variables.

    val root = Root.copy(collections = Map(
      rname -> ResolvedAst.Collection.Relation(rname, List(
        ResolvedAst.Attribute(x, Type.Unit),
        ResolvedAst.Attribute(y, Type.Bool),
        ResolvedAst.Attribute(z, Type.Int32),
        ResolvedAst.Attribute(w, Type.Str)
      ), SL)
    ))

    val rast =
      ResolvedAst.Predicate.Head.Relation(rname, List(
        ResolvedAst.Term.Head.Var(x, SL),
        ResolvedAst.Term.Head.Var(y, SL),
        ResolvedAst.Term.Head.Var(z, SL),
        ResolvedAst.Term.Head.Var(w, SL)
      ), SL)

    val expectedType = Type.Predicate(List(
      Type.Unit, Type.Bool, Type.Int32, Type.Str
    ))
    val actualType = Typer.Predicate.Head.typer(rast, root).get.tpe
    assertResult(expectedType)(actualType)
  }

  test("Predicate.Head03") {
    val relationName = Name.Resolved.mk(List("foo", "bar"))
    val functionName = Name.Resolved.mk(List("foo", "baz"))
    val x = ident("x")

    // NB: Somewhat misleading we use the same identifiers for both columns and variables.

    val root = Root.copy(
      constants = Map(
        functionName -> ResolvedAst.Definition.Constant(
          name = functionName,
          exp = ResolvedAst.Expression.Lambda(
            Ast.Annotations(List.empty),
            formals = List(ResolvedAst.FormalArg(x, Type.Bool)),
            retTpe = Type.Unit,
            body = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit(SL), SL), SL
          ),
          tpe = Type.Lambda(List(Type.Bool), Type.Unit), SL)
      ),
      collections = Map(
        relationName -> ResolvedAst.Collection.Relation(relationName, List(
          ResolvedAst.Attribute(x, Type.Unit)
        ), SL)
      ))

    val rast =
      ResolvedAst.Predicate.Head.Relation(relationName, List(
        ResolvedAst.Term.Head.Apply(
          functionName,
          List(ResolvedAst.Term.Head.Lit(
            ResolvedAst.Literal.Bool(true, SL), SL)), SL)), SL)

    val expectedType = Type.Predicate(List(Type.Unit))
    val actualType = Typer.Predicate.Head.typer(rast, root).get.tpe
    assertResult(expectedType)(actualType)
  }

  test("Predicate.Body01") {
    val rname = Name.Resolved.mk(List("foo", "bar"))
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")
    val w = ident("w")

    val root = Root.copy(collections = Map(
      rname -> ResolvedAst.Collection.Relation(rname, List(
        ResolvedAst.Attribute(x, Type.Unit),
        ResolvedAst.Attribute(y, Type.Bool),
        ResolvedAst.Attribute(z, Type.Int32),
        ResolvedAst.Attribute(w, Type.Str)
      ), SL)
    ))

    val rast =
      ResolvedAst.Predicate.Body.Relation(rname, List(
        ResolvedAst.Term.Body.Wildcard(SourceLocation.Unknown),
        ResolvedAst.Term.Body.Lit(ResolvedAst.Literal.Bool(true, SL), SL),
        ResolvedAst.Term.Body.Lit(ResolvedAst.Literal.Int(42, SL), SL),
        ResolvedAst.Term.Body.Lit(ResolvedAst.Literal.Str("foo", SL), SL)
      ), SL)

    val expectedType = Type.Predicate(List(
      Type.Unit, Type.Bool, Type.Int32, Type.Str
    ))
    val actualType = Typer.Predicate.Body.typer(rast, root).get.tpe
    assertResult(expectedType)(actualType)
  }

  // TODO: Test Term.Ascribe.


  test("NoSuchLattice01") {
    val input =
      s"""namespace A {
          |  lat A(x: Int, y: Int<>);
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Typer.TypeError.NoSuchLattice])
  }

  test("NoSuchLattice02") {
    val input =
      s"""namespace A {
          |  enum Elm {
          |    case Foo
          |  }
          |
         |  lat A(x: Int, y: Elm<>);
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Typer.TypeError.NoSuchLattice])
  }


  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  ignore("Type.Int8.Ascribe") {
    val input = "fn f(): Int8 = 42 : Int8"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int8.Plus") {
    val input = "fn f(x: Int8, y: Int8): Int8 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int8.Times") {
    val input = "fn f(x: Int8, y: Int8): Int8 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  ignore("Type.Int16.Ascribe") {
    val input = "fn f(): Int16 = 42 : Int16"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int16.Plus") {
    val input = "fn f(x: Int16, y: Int16): Int16 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int16.Times") {
    val input = "fn f(x: Int16, y: Int16): Int16 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Ascribe") {
    val input = "fn f(): Int32 = 42 : Int32"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Plus") {
    val input = "fn f(x: Int32, y: Int32): Int32 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int32.Times") {
    val input = "fn f(x: Int32, y: Int32): Int32 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  ignore("Type.Int64.Ascribe") {
    val input = "fn f(): Int64 = 42 : Int64"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int64.Plus") {
    val input = "fn f(x: Int64, y: Int64): Int64 = x + y"
    val result = new Flix().addStr(input).compile()
    result.get
  }

  test("Type.Int64.Times") {
    val input = "fn f(x: Int64, y: Int64): Int64 = x * y"
    val result = new Flix().addStr(input).compile()
    result.get
  }


  test("Definition.BoundedLattice.TypeError01") {
    val input =
      """let Int<> = (0, 1, 2, 3, 4);
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Typer.TypeError])
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
    assert(result.errors.head.isInstanceOf[Typer.TypeError])
  }

  test("Expression.Lambda.TypeError") {
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")
    val w = ident("w")

    val rast = ResolvedAst.Expression.Lambda(
      Ast.Annotations(List.empty),
      formals = List(
        ResolvedAst.FormalArg(x, Type.Unit),
        ResolvedAst.FormalArg(y, Type.Bool),
        ResolvedAst.FormalArg(z, Type.Int32),
        ResolvedAst.FormalArg(w, Type.Str)
      ),
      retTpe = Type.Unit,
      body = ResolvedAst.Expression.Var(w, SL)
      , SL)

    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }


  test("Pattern.TypeError") {
    val rast = ResolvedAst.Pattern.Lit(ResolvedAst.Literal.Unit(SL), SL)
    val tpe = Type.Bool
    val result = Typer.Pattern.typer(rast, tpe, Root)
    assert(result.isFailure)
  }
  test("Expression.Apply.TypeError.IllegalArgumentType") {
    val x = ident("x")
    val y = ident("y")
    val z = ident("z")

    val rast = ResolvedAst.Expression.Apply(
      lambda =
        ResolvedAst.Expression.Lambda(
          Ast.Annotations(List.empty),
          formals = List(
            ResolvedAst.FormalArg(x, Type.Bool),
            ResolvedAst.FormalArg(y, Type.Int32),
            ResolvedAst.FormalArg(z, Type.Str)
          ),
          retTpe = Type.Int32,
          body = ResolvedAst.Expression.Var(y, SL), SL
        ),
      args = List(
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo", SL), SL),
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42, SL), SL),
        ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true, SL), SL)
      ), SL)

    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }
}
