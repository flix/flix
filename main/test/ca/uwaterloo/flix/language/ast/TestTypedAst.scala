package ca.uwaterloo.flix.language.ast

import org.scalatest.FunSuite

class TestTypedAst extends FunSuite {

  val SL = SourceLocation.Unknown

  test("Pattern.Bound") {
    val x = ident("x")
    val y = ident("y")

    val pat = TypedAst.Pattern.Tuple(List(
      TypedAst.Pattern.Var(x, TypedAst.Type.Bool, SL),
      TypedAst.Pattern.Var(y, TypedAst.Type.Int, SL)
    ), TypedAst.Type.Tuple(List(TypedAst.Type.Bool, TypedAst.Type.Int)), SL)

    assertResult(Map(
      "x" -> TypedAst.Type.Bool,
      "y" -> TypedAst.Type.Int
    ))(pat.freeVars)
  }

  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
