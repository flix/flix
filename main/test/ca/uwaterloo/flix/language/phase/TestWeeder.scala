package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import org.scalatest.FunSuite


class TestWeeder extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Alias                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAlias01") {
    val input = "P(x, y) :- A(x), y := 21, y := 42. "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAlias])
  }

  test("DuplicateAlias02") {
    val input = "P(x, y) :- A(x), y := 21, z := 84, y := 42."
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAlias])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Annotation                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAnnotation01") {
    val input =
      """@strict @strict
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  test("DuplicateAnnotation02") {
    val input =
      """@strict @monotone @strict @monotone
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Attribute                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAttribute01") {
    val input = "rel A(x: Int, x: Int)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute02") {
    val input = "rel A(x: Int, y: Int, x: Int)   "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute03") {
    val input = "rel A(x: Bool, x: Int, x: Str)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Formal                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateFormal01") {
    val input = "fn f(x: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal02") {
    val input = "fn f(x: Int, y: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal03") {
    val input = "fn f(x: Bool, x: Int, x: Str): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Tag                                                           //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateTag01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateTag])
  }

  test("DuplicateTag02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateTag])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Annotation                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalAnnotation01") {
    val input =
      """@abc
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalAnnotation])
  }

  test("IllegalAnnotation02") {
    val input =
      """@foobarbaz
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalAnnotation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Body Term                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalBodyTerm01") {
    val input = "P(x) :- A(f(x))."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalBodyTerm])
  }

  test("IllegalBodyTerm02") {
    val input = "P(x) :- A(x), B(f(x)), C(x)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalBodyTerm])
  }

  test("IllegalBodyTerm03") {
    val input = "P(x, y) :- A(x, y), B(x `f` y)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalBodyTerm])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Head Term                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalHeadTerm01") {
    val input = "P(_)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadTerm])
  }

  test("IllegalHeadTerm02") {
    val input = "P(x, _, z) :- A(x, z)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadTerm])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Lattice                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalLattice01") {
    val input = "let Foo<> = (1, 2)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLattice])
  }

  test("IllegalLattice02") {
    val input = "let Foo<> = (1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLattice])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Lattice Attribute in Relation                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalLatticeAttributeInRelation01") {
    val input = "rel A(b: Int, c: Int<>)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  test("IllegalLatticeAttributeInRelation02") {
    val input = "rel A(b: Int<>, c: Int)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  test("IllegalLatticeAttributeInRelation03") {
    val input = "rel A(b: Int<>, c: Int<>)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLatticeAttributeInRelation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Lattice Attribute in Relation                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalNonLatticeAttribute01") {
    val input = "lat A(b: Int)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  test("IllegalNonLatticeAttribute02") {
    val input = "lat A(b: Int, c: Int)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  test("IllegalNonLatticeAttribute03") {
    val input = "lat A(b: Int, c: Int, d: Int)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalNonLatticeAttribute])
  }

  /////////////////////////////////////////////////////////////////////////////
  // IllegalMixedAttributes                                                  //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalMixedAttributes01") {
    val input = "lat A(b: Int, c: Int<>, d: Int, e: Int<>)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  test("IllegalMixedAttributes02") {
    val input = "lat A(b: Int, c: Int<>, d: Int, e: Int, f: Int<>)"
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  test("IllegalMixedAttributes03") {
    val input = "lat A(b: Int<>, c: Int, d: Int<>, e: Int, f: Int<>)"
    val past = new Parser(SourceInput.Str(input)).Definition.run().get
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalMixedAttributes])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Non Linear Pattern                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("NonLinearPattern01") {
    val input =
      """fn f(): Bool = match (21, 42) with {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
  }

  test("NonLinearPattern02") {
    val input =
      """fn f(): Bool = match (21, 42, 84) with {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
  }

  test("NonLinearPattern03") {
    val input =
      """fn f(): Bool = match (1, (2, (3, 4))) with {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
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


  // TODO: Use source code directly in tests.
  val SP = SourcePosition.Unknown
  val Ident = ident("x")

  def ident(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

}
