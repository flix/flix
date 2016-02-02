package ca.uwaterloo.flix.language.ast

import org.scalatest.FunSuite

class TestTypedAst extends FunSuite {

  val SL = SourceLocation.Unknown

  test("Pattern.Bound") {
    val x = ident("x")
    val y = ident("y")

    val pat = TypedAst.Pattern.Tuple(List(
      TypedAst.Pattern.Var(x, Type.Bool, SL),
      TypedAst.Pattern.Var(y, Type.Int32, SL)
    ), Type.Tuple(List(Type.Bool, Type.Int32)), SL)

    assertResult(Map(
      "x" -> Type.Bool,
      "y" -> Type.Int32
    ))(pat.freeVars)
  }

  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
