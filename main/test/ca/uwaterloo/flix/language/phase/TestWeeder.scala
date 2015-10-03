package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.immutable.Seq

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  val SP = SourcePosition.Unknown
  val Ident = ident("x")

  /////////////////////////////////////////////////////////////////////////////
  // Enums                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateTag01") {
    val past = ParsedAst.Definition.Enum(SP, Ident, Seq(
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit)
    ), SP)

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateTag02") {
    val past = ParsedAst.Definition.Enum(SP, Ident, Seq(
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("y"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit)
    ), SP)

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lattices                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalLattice01") {
    val past = ParsedAst.Definition.Lattice(SP, ParsedAst.Type.Unit, Seq(), Seq.empty, SP)
    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("IllegalLattice02") {
    val past = ParsedAst.Definition.Lattice(SP, ParsedAst.Type.Unit, Seq(
      ParsedAst.Expression.Error(SP, ParsedAst.Type.Unit, SP)
    ), Seq.empty, SP)
    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("IllegalLattice03") {
    val past = ParsedAst.Definition.Lattice(SP, ParsedAst.Type.Unit, Seq(
      ParsedAst.Expression.Error(SP, ParsedAst.Type.Unit, SP),
      ParsedAst.Expression.Error(SP, ParsedAst.Type.Unit, SP),
      ParsedAst.Expression.Error(SP, ParsedAst.Type.Unit, SP),
      ParsedAst.Expression.Error(SP, ParsedAst.Type.Unit, SP)
    ), Seq.empty, SP)
    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Relations                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAttribute01") {
    val past = ParsedAst.Definition.Relation(SP, Ident, Seq(
      ParsedAst.Attribute(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ident("x"), ParsedAst.Type.Unit)
    ), SP)

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateAttribute02") {
    val past = ParsedAst.Definition.Relation(SP, Ident, Seq(
      ParsedAst.Attribute(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ident("y"), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Attribute(ident("x"), ParsedAst.Type.Unit)
    ), SP)

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateFormal01") {
    val past = ParsedAst.Definition.Function(SP, Ident, Seq(
      ParsedAst.FormalArg(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(ident("x"), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SP, ParsedAst.Literal.Unit(SP, SP), SP), SP)

    val result = Weeder.Definition.compile(past)
    assert(result.hasErrors)
  }

  test("DuplicateFormal02") {
    val past = ParsedAst.Definition.Function(SP, Ident, Seq(
      ParsedAst.FormalArg(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(ident("y"), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.FormalArg(ident("x"), ParsedAst.Type.Unit)
    ), ParsedAst.Type.Unit, ParsedAst.Expression.Lit(SP, ParsedAst.Literal.Unit(SP, SP), SP), SP)

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

  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
