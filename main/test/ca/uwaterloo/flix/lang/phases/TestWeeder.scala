package ca.uwaterloo.flix.lang.phases

import ca.uwaterloo.flix.lang.ast.ParsedAst
import ca.uwaterloo.flix.lang.ast.SourceLocation
import ca.uwaterloo.flix.lang.phase.Weeder

import scala.collection.immutable.Seq

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  val Ident = ParsedAst.Ident("x", SourceLocation.Unknown)

  test("DuplicateTag01") {
    val past = ParsedAst.Declaration.Enum(Ident, Seq(
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateTag02") {
    val past = ParsedAst.Declaration.Enum(Ident, Seq(
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compile(past)
    assertResult(2)(result.errors.size)
  }


}
