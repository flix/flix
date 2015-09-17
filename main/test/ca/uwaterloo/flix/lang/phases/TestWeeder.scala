package ca.uwaterloo.flix.lang.phases

import ca.uwaterloo.flix.lang.ast.{WeededAst, ParsedAst, SourceLocation}
import ca.uwaterloo.flix.lang.phase.{Parser, Weeder}

import scala.collection.immutable.Seq

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  val Ident = ParsedAst.Ident("x", SourceLocation.Unknown)

  test("DuplicateAttribute01") {
    val past = ParsedAst.Definition.Relation(Ident, Seq(
      ParsedAst.Attribute(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compileDefinition(past)
    assert(result.hasErrors)
  }

  test("DuplicateAttribute02") {
    val past = ParsedAst.Definition.Relation(Ident, Seq(
      ParsedAst.Attribute(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ParsedAst.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compileDefinition(past)
    assertResult(2)(result.errors.size)
  }

  test("DuplicateArgument01") {
    val past = ParsedAst.Definition.Function(Ident, Seq(
      (ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      (ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(ParsedAst.Literal.Unit))

    val result = Weeder.compileDefinition(past)
    assert(result.hasErrors)
  }

  test("DuplicateArgument02") {
    val past = ParsedAst.Definition.Function(Ident, Seq(
      (ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      (ParsedAst.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      (ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      (ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(ParsedAst.Literal.Unit))

    val result = Weeder.compileDefinition(past)
    assertResult(2)(result.errors.size)
  }


  test("DuplicateTag01") {
    val past = ParsedAst.Definition.Enum(Ident, Seq(
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compileEnum(past)
    assert(result.hasErrors)
  }

  test("DuplicateTag02") {
    val past = ParsedAst.Definition.Enum(Ident, Seq(
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ParsedAst.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.compileEnum(past)
    assertResult(2)(result.errors.size)
  }

  test("NonLinearPattern01") {
    val input = "(x, x)"
    val past = new Parser(None, input).Pattern.run().get
    val result = Weeder.compilePattern(past)
    assert(result.isFailure)
    assertResult(1)(result.errors.size)
  }

  test("NonLinearPattern02") {
    val input = "(x, (y, (z, x, y)))"
    val past = new Parser(None, input).Pattern.run().get
    val result = Weeder.compilePattern(past)
    assert(result.isFailure)
    assertResult(2)(result.errors.size)
  }

  test("ApplyNotAllowInBody01") {
    val input = "A(x) :- B(f(x))."
    val past = new Parser(None, input).RuleDeclaration.run().get
    val result = Weeder.compileRule(past)
    assert(result.isFailure)
    assertResult(1)(result.errors.size)
  }

  test("ApplyNotAllowInBody02") {
    val input = "A(x) :- B(x), C(f(x)), D(g(x))."
    val past = new Parser(None, input).RuleDeclaration.run().get
    val result = Weeder.compileRule(past)
    assert(result.isFailure)
    assertResult(2)(result.errors.size)
  }

  test("Compile.TermNoApply") {
    val past = ParsedAst.Term.Apply(ParsedAst.QName(Seq("foo"), SourceLocation.Unknown), Seq.empty)
    val result = Weeder.compileTermNoApply(past)
    assert(result.isFailure)
  }

  test("Compile.TermWithApply") {
    val past = ParsedAst.Term.Apply(ParsedAst.QName(Seq("foo"), SourceLocation.Unknown), Seq.empty)
    val result = Weeder.compileTermWithApply(past)
    assert(result.isSuccess)
  }

  test("Type.Unit") {
    val past = ParsedAst.Type.Unit
    val result = Weeder.Type.weed(past)

    assertResult(WeededAst.Type.Unit)(result.get)
  }

  test("Type.Tag") {
    val past = ParsedAst.Type.Tag(Ident, ParsedAst.Type.Unit)
    val result = Weeder.Type.weed(past)

    assertResult(WeededAst.Type.Tag(Ident, WeededAst.Type.Unit))(result.get)
  }

}
