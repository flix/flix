package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.immutable.Seq

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  val SL = SourceLocation.Unknown
  val SP = SourcePosition.Unknown
  val Ident = Name.Ident("x", SourceLocation.Unknown)

  /////////////////////////////////////////////////////////////////////////////
  // Enums                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateTag01") {
    val past = ParsedAst.Definition.Enum(SL, Ident, Seq(
      ParsedAst.Type.Tag(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateTag02") {
    val past = ParsedAst.Definition.Enum(SL, Ident, Seq(
      ParsedAst.Type.Tag(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(Name.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Relations                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAttribute01") {
    val past = ParsedAst.Definition.Relation(SL, Ident, Seq(
      ParsedAst.Attribute(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateAttribute02") {
    val past = ParsedAst.Definition.Relation(SL, Ident, Seq(
      ParsedAst.Attribute(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(Name.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.Attribute(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ))

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateFormal01") {
    val past = ParsedAst.Definition.Function(SL, Ident, Seq(
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SL, ParsedAst.Literal.Unit(SP, SP)))

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateFormal02") {
    val past = ParsedAst.Definition.Function(SL, Ident, Seq(
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SL, ParsedAst.Literal.Unit(SP, SP)))

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }

  test("NonLinearPattern01") {
    val input = "(x, x)"
    val past = new Parser(SourceInput.Str(input)).Pattern.run().get
    val result = Weeder.Pattern.compile(past)
    assert(result.isFailure)
    assertResult(1)(result.errors.size)
  }

  test("NonLinearPattern02") {
    val input = "(x, (y, (z, x, y)))"
    val past = new Parser(SourceInput.Str(input)).Pattern.run().get
    val result = Weeder.Pattern.compile(past)
    assert(result.isFailure)
    assertResult(2)(result.errors.size)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Term.Head.Wildcard01") {
    val input = "A(_)."
    val past = new Parser(SourceInput.Str(input)).FactDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("Term.Head.Wildcard02") {
    val input = "A(_) :- B(x)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("Term.Body.Apply01") {
    val input = "A(x) :- B(f(x))."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("Term.Body.Apply02") {
    val input = "A(x) :- B(x), C(f(x)), D(g(x))."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

}
