package ca.uwaterloo.flix.lang.phases

import ca.uwaterloo.flix.lang.ast.{TypedAst, ResolvedAst}
import ca.uwaterloo.flix.lang.phase.Typer
import org.scalatest.FunSuite

class TestTyper extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Literal.Unit") {
    val rast = ResolvedAst.Literal.Unit
    val result = Typer.Literal.typer(rast).get
    assertResult(TypedAst.Type.Unit)(result.tpe)
  }

  test("Literal.Tuple") {
    val rast = ResolvedAst.Literal.Tuple(Seq(ResolvedAst.Literal.Bool(true), ResolvedAst.Literal.Int(42)))
    val result = Typer.Literal.typer(rast).get
    assertResult(TypedAst.Type.Tuple(Seq(TypedAst.Type.Bool, TypedAst.Type.Int)))(result.tpe)
  }

}
