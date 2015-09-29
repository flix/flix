package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{Name, WeededAst, ParsedAst, SourceLocation}

import scala.collection.immutable.Seq

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  val SL = SourceLocation.Unknown
  val Ident = Name.Ident("x", SourceLocation.Unknown)

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

  test("DuplicateFormal01") {
    val past = ParsedAst.Definition.Function(SL, Ident, Seq(
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SL, ParsedAst.Literal.Unit(SL)))

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateFormal02") {
    val past = ParsedAst.Definition.Function(SL, Ident, Seq(
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("y", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(Name.Ident("x", SourceLocation.Unknown), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SL, ParsedAst.Literal.Unit(SL)))

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }


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

  test("NonLinearPattern01") {
    val input = "(x, x)"
    val past = new Parser(None, input).Pattern.run().get
    val result = Weeder.Pattern.compile(past)
    assert(result.isFailure)
    assertResult(1)(result.errors.size)
  }

  test("NonLinearPattern02") {
    val input = "(x, (y, (z, x, y)))"
    val past = new Parser(None, input).Pattern.run().get
    val result = Weeder.Pattern.compile(past)
    assert(result.isFailure)
    assertResult(2)(result.errors.size)
  }

  test("ApplyNotAllowInBody01") {
    val input = "A(x) :- B(f(x))."
    val past = new Parser(None, input).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
    assertResult(1)(result.errors.size)
  }

  // TODO: Wildcard not allowed in head.

  // TODO: Rename

  test("ApplyNotAllowInBody02") {
    val input = "A(x) :- B(x), C(f(x)), D(g(x))."
    val past = new Parser(None, input).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
    assertResult(2)(result.errors.size)
  }



  test("Term.Heead") {
    val past = ParsedAst.Term.Apply(SL, Name.Unresolved(List("foo"), SourceLocation.Unknown), Seq.empty)
    val result = Weeder.Term.Head.compile(past)
    assert(result.isSuccess)
  }

  test("Term.Body") {
    val past = ParsedAst.Term.Apply(SL, Name.Unresolved(List("foo"), SourceLocation.Unknown), Seq.empty)
    val result = Weeder.Term.Body.compile(past)
    assert(result.isFailure)
  }

  test("Type.Unit") {
    val past = ParsedAst.Type.Unit
    val result = Weeder.Type.compile(past)

    assertResult(WeededAst.Type.Unit)(result.get)
  }

  test("Type.Tag") {
    val past = ParsedAst.Type.Tag(Ident, ParsedAst.Type.Unit)
    val result = Weeder.Type.compile(past)

    assertResult(WeededAst.Type.Tag(Ident, WeededAst.Type.Unit))(result.get)
  }

}
