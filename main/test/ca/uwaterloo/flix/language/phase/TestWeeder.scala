package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.Flix
import ca.uwaterloo.flix.Flix.Builder
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class TestWeeder extends FunSuite {

  // TODO: Use source code directly in tests.
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
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateTag])
  }

  test("DuplicateTag02") {
    val past = ParsedAst.Definition.Enum(SP, Ident, Seq(
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("y"), ParsedAst.Type.Unit),
      ParsedAst.Type.Tag(ident("x"), ParsedAst.Type.Unit)
    ), SP)

    val result = Weeder.Definition.compile(past)
    assert(result.isFailure)
  }

  /////////////////////////////////////////////////////////////////////////////
  // BoundedLattices                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalBoundedLattice01") {
    val input = "let Foo<> = (bot)"
    val past = new Parser(SourceInput.Str(input)).BoundedLatticeDefinition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalBoundedLattice02") {
    val input = "let Foo<> = (bot, top)"
    val past = new Parser(SourceInput.Str(input)).BoundedLatticeDefinition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalBoundedLattice03") {
    val input = "let Foo<> = (1, 2, 3, 4, 5, 6, 7)"
    val past = new Parser(SourceInput.Str(input)).BoundedLatticeDefinition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lattices and Relations                                                  //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAttribute01") {
    val past = ParsedAst.Definition.Relation(SP, Ident, Seq(
      ParsedAst.Attribute(ident("x"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit)),
      ParsedAst.Attribute(ident("x"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit))
    ), SP)

    val result = Weeder.Definition.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute02") {
    val past = ParsedAst.Definition.Relation(SP, Ident, Seq(
      ParsedAst.Attribute(ident("x"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit)),
      ParsedAst.Attribute(ident("y"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit)),
      ParsedAst.Attribute(ident("x"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit)),
      ParsedAst.Attribute(ident("x"), ParsedAst.Interpretation.Set(ParsedAst.Type.Unit))
    ), SP)

    val result = Weeder.Definition.compile(past)
    assertResult(2)(result.errors.size)
  }

  test("IllegalAttribute01") {
    val input = "rel A(b: Int, c: Int<>)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  test("IllegalAttribute02") {
    val input = "rel A(b: Int<>, c: Int)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  test("IllegalAttribute03") {
    val input = "rel A(b: Int<>, c: Int<>)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  test("IllegalNonLatticeAttribute01") {
    val input = "lat A(b: Int)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  test("IllegalNonLatticeAttribute02") {
    val input = "lat A(b: Int, c: Int)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  test("IllegalNonLatticeAttribute03") {
    val input = "lat A(b: Int, c: Int, d: Int)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  test("IllegalMixedAttributes01") {
    val input = "lat A(b: Int, c: Int<>, d: Int, e: Int<>)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  test("IllegalMixedAttributes02") {
    val input = "lat A(b: Int, c: Int<>, d: Int, e: Int, f: Int<>)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  test("IllegalMixedAttributes03") {
    val input = "lat A(b: Int<>, c: Int, d: Int<>, e: Int, f: Int<>)."
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateFormal01") {
    val input = "fn f(x: Int, x: Int): Int = 42"
    val result = new Flix.Builder().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal02") {
    val input = "fn f(x: Int, y: Int, x: Int): Int = 42"
    val result = new Flix.Builder().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal03") {
    val input = "fn f(x: Bool, x: Int, x: Str): Int = 42"
    val result = new Flix.Builder().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
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
  // Predicates, Facts and Rules                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalHeadPredicate.Alias01") {
    val input = "x := y."
    val past = new Parser(SourceInput.Str(input)).FactDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalHeadPredicate.Alias02") {
    val input = "x := y :- A(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalHeadPredicate.NotEqual01") {
    val input = "x != y."
    val past = new Parser(SourceInput.Str(input)).FactDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalHeadPredicate.NotEqual02") {
    val input = "x != y :- A(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalHeadPredicate.Read01") {
    val input = "Read#(x, y)."
    val past = new Parser(SourceInput.Str(input)).FactDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalHeadPredicate.Read02") {
    val input = "Read#(x, y) :- A(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalBodyPredicate.Trace01") {
    val input = "A(x, y) :- Trace#(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalBodyPredicate.Write01") {
    val input = "A(x, y) :- Write#(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalBodyPredicate.Error01") {
    val input = "A(x, y) :- Error#(x, y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalReadPredicate01") {
    val input = "A(x, y) :- Read#(\"a.txt\")."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalWritePredicate01") {
    val input = "Write#(\"a.txt\") :- A(x)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
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

  test("Term.Body.Apply03") {
    val input = "A(x) :- B(x `plus` y)."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalAlias01") {
    val input = "P(x) :- x := 42, x := 21"
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  test("IllegalAlias02") {
    val input = "P(x) :- x := _."
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isFailure)
  }

  ignore("RuleIsFact01") {
    val input = "P(x) :- x := 42"
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isInstanceOf[WeededAst.Declaration.Fact])
  }

  ignore("RuleIsFact02") {
    val input = "P(x, y) :- x := 21, y := 42"
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isInstanceOf[WeededAst.Declaration.Fact])
  }

  ignore("RuleIsFact03") {
    val input = "P(f(x, y)) :- x := 21, y := 42"
    val past = new Parser(SourceInput.Str(input)).RuleDeclaration.run().get
    val result = Weeder.Declaration.compile(past)
    assert(result.isInstanceOf[WeededAst.Declaration.Fact])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Annotations                                                             //
  /////////////////////////////////////////////////////////////////////////////

  test("DuplicateAnnotation01") {
    val input =
      """@strict @strict
        |fn foo(x: Int): Int = 42
        |
      """.stripMargin
    val result = new Flix.Builder().addStr(input).solve()

    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  test("DuplicateAnnotation02") {
    val input =
      """@strict @monotone @strict @monotone
        |fn foo(x: Int): Int = 42
        |
      """.stripMargin
    val result = new Flix.Builder().addStr(input).solve()

    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  test("IllegalAnnotation01") {
    val input =
      """@foobar
        |fn foo(x: Int): Int = 42
        |
      """.stripMargin
    val result = new Flix.Builder().addStr(input).solve()

    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalAnnotation])
  }


  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
