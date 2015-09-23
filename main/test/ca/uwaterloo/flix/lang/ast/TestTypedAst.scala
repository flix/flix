package ca.uwaterloo.flix.lang.ast

import org.scalatest.FunSuite

class TestTypedAst extends FunSuite {

  test("Pattern.Bound") {
    val x = ParsedAst.Ident("x", SourceLocation.Unknown)
    val y = ParsedAst.Ident("y", SourceLocation.Unknown)

    val pat = TypedAst.Pattern.Tuple(List(
      TypedAst.Pattern.Var(x, TypedAst.Type.Bool),
      TypedAst.Pattern.Var(y, TypedAst.Type.Int)
    ), TypedAst.Type.Tuple(List(TypedAst.Type.Bool, TypedAst.Type.Int)))

    assertResult(Map(
      x -> TypedAst.Type.Bool,
      y -> TypedAst.Type.Int
    ))(pat.bound)
  }

}
