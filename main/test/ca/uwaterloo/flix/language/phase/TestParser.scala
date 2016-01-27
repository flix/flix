package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ParsedAst.Literal
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import org.scalatest.FunSuite

// TODO: Cleanup names. Numbering and remove the Parser. prefix.

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("Root01") {
    val input = ""
    val result = new Parser(SourceInput.Str(input)).Root.run().get
    assert(result.isInstanceOf[ParsedAst.Root])
  }

  test("Root02") {
    val input =
      """namespace a {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Root.run().get
    assert(result.isInstanceOf[ParsedAst.Root])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations and Definitions                                            //
  /////////////////////////////////////////////////////////////////////////////
  test("Declaration.Namespace01") {
    val input =
      """namespace foo {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).NamespaceDeclaration.run().get
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Declaration.Namespace02") {
    val input =
      """namespace foo::bar::baz {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).NamespaceDeclaration.run().get
    assertResult(Seq("foo", "bar", "baz"))(result.name.parts)
  }

  test("Declaration.Namespace03") {
    val input =
      """namespace foo {
        |  namespace bar {
        |    namespace baz {
        |      // a comment
        |    }
        |  }
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).NamespaceDeclaration.run()
    assert(result.isSuccess)
  }

  test("Definition.Value01") {
    val input = "val v: Int = 42;"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Value])
  }

  test("Definition.Value02") {
    val input = "val v: Int = 1 + 1;"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Value])
  }

  test("Definition.Function01") {
    val input = "def foo(): Int = 42"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Definition.Function02") {
    val input = "def foo(x: Int): Int = 42"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Definition.Function03") {
    val input = "def foo(x: Int, y: Int, z: Int): Int = x + y + z"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Definition.Enum01") {
    val input =
      """enum A {
        |  case B
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  test("Definition.Enum02") {
    val input =
      """enum A {
        |  case B(Int)
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  test("Definition.Enum03") {
    val input =
      """enum A {
        |  case B,
        |  case C(Int),
        |  case D(Bool, Int, Str)
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  // TODO: Allow naming of the enum attributes.

  test("Definition.BoundedLattice01") {
    val input = "let a<> = (Tag.Bot, Tag.Top, leq, lub, glb)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.BoundedLattice])
  }

  test("Definition.BoundedLattice02") {
    val input = "let a<> = (Tag.Bot, Tag.Top, foo::leq, bar::lub, baz::qux::glb)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.BoundedLattice])
  }

  test("Definition.BoundedLattice03") {
    val input = "let a<> = (Tag.Bot, Tag.Top, fn (x: Int, y: Int): Bool = true, bar::lub, baz::qux::glb)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.BoundedLattice])
  }

  test("Definition.Relation01") {
    val input = "rel A(b: B)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Relation])
  }

  test("Definition.Relation02") {
    val input = "rel A(b: B, c: C, d: D)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Relation])
  }

  test("Definition.Lattice01") {
    val input = "lat A(b: B<>)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Definition.Lattice02") {
    val input = "lat A(b: B, c: C<>, d: D<>)"
    val result = new Parser(SourceInput.Str(input)).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Definition.Index01") {
    val input = "index A({x});"
    val result = new Parser(SourceInput.Str(input)).IndexDefinition.run()
    assert(result.isSuccess)
  }

  test("Definition.Index02") {
    val input = "index A({x}, {x, y});"
    val result = new Parser(SourceInput.Str(input)).IndexDefinition.run()
    assert(result.isSuccess)
  }

  test("Definition.Index03") {
    val input = "index A({x}, {y}, {x, w}, {x, y, z, w});"
    val result = new Parser(SourceInput.Str(input)).IndexDefinition.run()
    assert(result.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Directives                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Directive.Assert01") {
    val input = "assert P(42)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertFact])
  }

  test("Directive.Assert02") {
    val input = "assert P(21, 42, \"foo\")."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertFact])
  }

  test("Directive.Assert03") {
    val input = "assert P(1, f(2))."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertFact])
  }

  test("Directive.Assert04") {
    val input = "assert Error#(x) :- A(x)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertRule])
  }

  test("Directive.Assert05") {
    val input = "assert Error#(x, z) :- A(x, y), B(y, z)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertRule])
  }

  test("Directive.Assert06") {
    val input = "assert Error#(x, f(z)) :- A(x, y), B(y, z)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertRule])
  }

  test("Directive.Assert07") {
    val input = "assert H(x) :- A(x)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertRule])
  }

  test("Directive.Assert08") {
    val input = "assert H(x, z) :- A(x, y), B(y, z)."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.AssertRule])
  }

  test("Directive.Print01") {
    val input = "print H."
    val result = new Parser(SourceInput.Str(input)).Declaration.run().get
    assert(result.isInstanceOf[ParsedAst.Directive.Print])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Check up on associativity.

  test("Expression.LogicalExp01") {
    val input = "true && false"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.LogicalAnd)(result.op)
  }

  test("Expression.LogicalExp02") {
    val input = "true || false"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.LogicalOr)(result.op)
  }

  test("Expression.LogicalExp03") {
    val input = "1 < 2 && 3 < 4"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.LogicalAnd)(result.op)
  }

  test("Expression.ComparisonExp01") {
    val input = "1 < 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Less)(result.op)
  }

  test("Expression.ComparisonExp02") {
    val input = "1 + 2 > 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Greater)(result.op)
  }

  test("Expression.ComparisonExp03") {
    val input = "1 + 2 > 3 + 4"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Greater)(result.op)
  }

  test("Expression.MultiplicativeExp01") {
    val input = "1 * 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Times)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Expression.MultiplicativeExp02") {
    val input = "1 * 2 * 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Times)(result.op)
  }

  test("Expression.MultiplicativeExp03") {
    val input = "1 * 2 + 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Expression.MultiplicativeExp04") {
    val input = "1 + 2 * 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Expression.AdditiveExp01") {
    val input = "1 + 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Expression.AdditiveExp02") {
    val input = "1 + 2 + 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Expression.AdditiveExp03") {
    val input = "1 - 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Expression.AdditiveExp04") {
    val input = "1 - 2 - 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(result.op)
    val e1 = result.e1.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(e1.op)
    assert(e1.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(e1.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Expression.AdditiveExp05") {
    val input = "1 + 2 - 3 + 4 - 5 + 6"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
  }

  test("Expression.Infix01") {
    val input = "1 `plus` 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assertResult(Seq("plus"))(result.name.parts)
  }

  test("Expression.Infix02") {
    val input = "1 `foo::bar::baz::plus` 2"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assertResult(Seq("foo", "bar", "baz", "plus"))(result.name.parts)
  }

  test("Expression.Infix03") {
    val input = "+1 `plus` -1"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Unary])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Unary])
  }

  test("Expression.UnaryExp01") {
    val input = "+ 1"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.Plus)(result.op)
  }

  test("Expression.UnaryExp02") {
    val input = "- 1"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.Minus)(result.op)
  }

  test("Expression.UnaryExp03") {
    val input = "!! true"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.LogicalNot)(result.op)
  }

  test("Expression.Ascribe01") {
    val input = "true: Bool"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Ascribe])
  }

  test("Expression.Ascribe02") {
    val input = "x: Bool -> Int"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Ascribe])
  }

  test("Expression.LiteralExp01") {
    val input = "true"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("true")(result.lit.asInstanceOf[ParsedAst.Literal.Bool].lit)
  }

  test("Expression.LiteralExp02") {
    val input = "42"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("42")(result.lit.asInstanceOf[ParsedAst.Literal.Int].lit)
  }

  test("Expression.LiteralExp03") {
    val input = "\"foo\""
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("foo")(result.lit.asInstanceOf[ParsedAst.Literal.Str].lit)
  }

  test("Expression.LetExp01") {
    val input = "let x = 42 in x"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("x")(result.ident.name)
  }

  test("Expression.LetExp02") {
    val input = "let x' = f(1, 2, 3) in g(4, 5, 6)"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("x'")(result.ident.name)
  }

  test("Expression.LetExp03") {
    val input =
      """let x = 1 in
        |let y = 2 in
        |let z = 3 in
        |  42""".stripMargin
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    val l1 = result.get.asInstanceOf[ParsedAst.Expression.Let]
    val l2 = l1.body.asInstanceOf[ParsedAst.Expression.Let]
    val l3 = l2.body.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("z")(l3.ident.name)
  }

  test("Expression.IfThenElseExp01") {
    val input = "if (1) 2 else 3"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Expression.IfThenElseExp02") {
    val input = "if (f(1, 2, 3)) g(4, 5, 6) else h(7, 8, 9)"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Expression.IfThenElseExp03") {
    val input = "if ((1)) (2) else (3)"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Expression.Switch01") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case true  => 1
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.isSuccess)
  }

  test("Expression.Switch02") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case x < 0  => 1
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.isSuccess)
  }

  test("Expression.Switch03") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case x < 0  => 1
        |  case x > 0  => 2
        |  case x == 0 => 3
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.isSuccess)
  }

  test("Expression.MatchExp01") {
    val input =
      """match 1 with {
        |  case 2 => 3
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Match])
  }

  test("Expression.MatchExp02") {
    val input =
      """match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Match])
  }

  test("Expression.MatchExp03") {
    val input =
      """match 1 with {
        |  case 2 => match 3 with {
        |    case 4 => 5
        |  }
        |  case 6 => 7
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    val m1 = result.asInstanceOf[ParsedAst.Expression.Match]
    val m2 = m1.rules.head._2.asInstanceOf[ParsedAst.Expression.Match]
    val l = m2.rules.head._2.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("5")(l.lit.asInstanceOf[ParsedAst.Literal.Int].lit)
  }

  test("Expression.MatchExp04") {
    val input =
      """match
        |  match 1 with {
        |    case 2 => 3
        |  } with {
        |    case 4 => 5
        |}
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    val m1 = result.asInstanceOf[ParsedAst.Expression.Match]
    val m2 = m1.e.asInstanceOf[ParsedAst.Expression.Match]
    val l = m2.rules.head._2.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("3")(l.lit.asInstanceOf[ParsedAst.Literal.Int].lit)
  }

  test("Expression.CallExp01") {
    val input = "f()"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Apply])
  }

  test("Expression.CallExp02") {
    val input = "f(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Apply])
  }

  test("Expression.CallExp03") {
    val input = "f(f(1), f(f(2)))"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Apply])
  }

  test("Expression.CallExp04") {
    val input = "foo::bar::baz::f(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Apply])
  }

  test("Expression.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Expression.Tag02") {
    val input = "Foo.Bar ()"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Expression.Tag03") {
    val input = "Foo.Bar Baz.Qux"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Expression.Tag04") {
    val input = "Foo.Bar (x, y)"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Expression.Tag05") {
    val input = "foo::bar::Baz.Qux (42, x, (3, 4))"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Expression.Tuple01") {
    val input = "()"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assert(result.lit.isInstanceOf[ParsedAst.Literal.Unit])
  }

  test("Expression.Tuple02") {
    val input = "(1)"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    val literal = result.lit.asInstanceOf[Literal.Int]
    assertResult("1")(literal.lit)
  }

  test("Expression.Tuple03") {
    val input = "(1, x)"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(2)(result.elms.size)
  }

  test("Expression.Tuple04") {
    val input = "(1, 2, x, 4, 5, 6)"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(6)(result.elms.size)
  }

  test("Expression.Tuple05") {
    val input = "((1, 2), (x, (4, 5)))"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(2)(result.elms.size)
  }

  test("Expression.Set01") {
    val input = "#{}"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Set])
  }

  test("Expression.Set02") {
    val input = "#{1, 2, 3}"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Set])
  }

  test("Expression.Set03") {
    val input = "#{(1, 2), (2, 3)}"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Set])
  }

  test("Expression.Set04") {
    val input = "#{1 + 2, 3 + 4}"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Set])
  }

  test("Expression.Set05") {
    val input = "#{#{1}, #{2}}"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Set])
  }

  test("Expression.Var01") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Var])
  }

  test("Expression.Var02") {
    val input = "foo::bar::baz::x_y_z''"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Var])
  }

  test("Expression.Lambda01") {
    val input = "fn(x: Int): Int = 42"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lambda]
    assert(result.body.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Expression.Lambda02") {
    val input = "fn(x: Bool, y: Int, z: Str): Str = x + y + z"
    val result = new Parser(SourceInput.Str(input)).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lambda]
    assert(result.body.isInstanceOf[ParsedAst.Expression.Binary])
  }

  test("Expression.ErrorExp01") {
    val input = "??? : Int"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Error])
  }

  test("Expression.Bot01") {
    val input = "⊥"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Bot])
  }

  test("Expression.Top01") {
    val input = "⊤"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Top])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Pattern.Wildcard") {
    val input = "_"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Wildcard])
  }

  test("Pattern.Var01") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Var]
    assertResult("x")(result.ident.name)
  }

  test("Pattern.Var02") {
    val input = "foo_Bar'"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Var]
    assertResult("foo_Bar'")(result.ident.name)
  }

  test("Pattern.Literal01") {
    val input = "true"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("true")(result.lit.asInstanceOf[ParsedAst.Literal.Bool].lit)
  }

  test("Pattern.Literal02") {
    val input = "42"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("42")(result.lit.asInstanceOf[ParsedAst.Literal.Int].lit)
  }

  test("Pattern.Literal03") {
    val input = "\"foo\""
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("foo")(result.lit.asInstanceOf[ParsedAst.Literal.Str].lit)
  }

  test("Pattern.Tag01") {
    val input = "Const.Bot"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag02") {
    val input = "Const.Cst(5)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag03") {
    val input = "Foo.Bar (x, _, z)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag04") {
    val input = "foo::bar::baz.Foo(x, y, z)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tuple01") {
    val input = "(x, y, true)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Tuple]
    assertResult(3)(result.elms.size)
  }

  test("Pattern.TaggedTuple01") {
    val input = "Foo.Bar(x)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Tag]
    assert(result.p.isInstanceOf[ParsedAst.Pattern.Var])
  }


  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("FactDeclaration01") {
    val input = "P(42)."
    val result = new Parser(SourceInput.Str(input)).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("FactDeclaration02") {
    val input = "P(\"foo\")."
    val result = new Parser(SourceInput.Str(input)).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("FactDeclaration03") {
    val input = "P(f(1, 2, 3))."
    val result = new Parser(SourceInput.Str(input)).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("RuleDeclaration01") {
    val input = "P(x) :- A(x)."
    val result = new Parser(SourceInput.Str(input)).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(1)(result.get.body.size)
  }

  test("RuleDeclaration02") {
    val input = "P(x, y, z) :- A(x), B(y), C(z)."
    val result = new Parser(SourceInput.Str(input)).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(3)(result.get.body.size)
  }

  test("RuleDeclaration03") {
    val input = "P(f(x), g(y, z)) :- isFoo(x, y), isBar(y, z), A(x), B(y), C(z)."
    val result = new Parser(SourceInput.Str(input)).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(5)(result.get.body.size)
  }

  test("Rule.Loop01") {
    val input = "P(x, z) :- A(x, y), z <- f(y)."
    val result = new Parser(SourceInput.Str(input)).RuleDeclaration.run()
    assert(result.isSuccess)
  }

  test("Predicate.Alias01") {
    val input = "r := 42"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Alias])
  }

  test("Predicate.Alias02") {
    val input = "r := (true, 42, \"foo\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Alias])
  }

  test("Predicate.Alias03") {
    val input = "r := f(x, g(y, z))"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Alias])
  }

  test("Predicate.Loop01") {
    val input = "y <- f(x)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Loop])
  }

  test("Predicate.Loop02") {
    val input = "x <- f(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Loop])
  }

  test("Predicate.Trace01") {
    val input = "Trace#(x)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Trace])
  }

  test("Predicate.Trace02") {
    val input = "Trace#(x, z)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Trace])
  }

  test("Predicate.Trace03") {
    val input = "Trace#(x, f(z))"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Trace])
  }

  test("Predicate.Read01") {
    val input = "Read#(x, \"a.csv\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Read])
  }

  test("Predicate.Read02") {
    val input = "Read#(x, y, z, \"a.csv\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Read])
  }

  test("Predicate.Write01") {
    val input = "Write#(x, \"a.csv\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Write])
  }

  test("Predicate.Write02") {
    val input = "Write#(x, y, z, \"a.csv\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Write])
  }

  test("Predicate.Error01") {
    val input = "Error#(42)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Error])
  }

  test("Predicate.Error02") {
    val input = "Error#(x, y, z)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Error])
  }

  test("Predicate.Error03") {
    val input = "Error#(42, f(x, g(y, z)))"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Error])
  }

  test("Predicate.NotEqual01") {
    val input = "x != y"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Term01") {
    val input = "_"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Wildcard])
  }

  test("Term02") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Var]
    assertResult("x")(result.ident.name)
  }

  test("Term03") {
    val input = "42"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assertResult("42")(result.lit.asInstanceOf[ParsedAst.Literal.Int].lit)
  }

  test("Term04") {
    val input = "foo(x)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Term05") {
    val input = "foo::bar(x, y, z)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    assertResult(Seq("foo", "bar"))(result.name.parts)
    assertResult(Seq("x", "y", "z"))(result.args.map(_.asInstanceOf[ParsedAst.Term.Var].ident.name))
  }

  test("Term.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.lit.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Term.Tag02") {
    val input = "Foo.Bar(1)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.lit.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Term.Tag03") {
    val input = "foo::bar::Baz.Bar(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.lit.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Term.Ascribe01") {
    val input = "(): Unit"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Ascribe])
  }

  test("Term.Ascribe02") {
    val input = "true: Bool"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Ascribe])
  }

  test("Term.Ascribe03") {
    val input = "42: Int"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Ascribe])
  }

  test("Term.Ascribe04") {
    val input = "\"foo\": Str"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Ascribe])
  }

  test("Term.Ascribe05") {
    val input = "((1: Int): Int) : Int"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Ascribe])
  }

  test("Term.Infix01") {
    val input = "1 `plus` 2"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Infix])
  }

  test("Term.Infix02") {
    val input = "x `plus` y"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Infix])
  }

  test("Term.Infix03") {
    val input = "Foo.Bar `plus` Baz.Qux"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Infix])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Function01") {
    val input = "(A) -> B"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function02") {
    val input = "(A, B) -> C"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function03") {
    val input = "((A, B)) -> C"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function04") {
    val input = "(A) -> (B, C)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function05") {
    val input = "(A) -> (B) -> C"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function06") {
    val input = "(A) -> ((B) -> C)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function07") {
    val input = "((A) -> B) -> C"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function08") {
    val input = "(A, B, C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function09") {
    val input = "((A, B), C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals(0).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals(0).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.formals(1).isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function10") {
    val input = "(((A, B), C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(0).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(0).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(1).isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function11") {
    val input = "(A, (B, C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals(0).isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals(1).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals(1).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function12") {
    val input = "((A, (B, C))) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(0).isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(1).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms(1).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function13") {
    val input = "((A, B, C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function14") {
    val input = "(A, B) -> (C, D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function15") {
    val input = "((A, B)) -> (C, D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function16") {
    val input = "(A) -> (B, C, D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function17") {
    val input = "(A) -> ((B, C), D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(0).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(0).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(1).isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function18") {
    val input = "(A) -> (B, (C, D))"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(0).isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(1).isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms(1).asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function19") {
    val input = "(A, B) -> (C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function20") {
    val input = "(A, B) -> ((C) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function21") {
    val input = "((A, B) -> C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function22") {
    val input = "((A, B)) -> (C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function23") {
    val input = "((A, B)) -> ((C) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function24") {
    val input = "(((A, B)) -> C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function25") {
    val input = "(A) -> (B, C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function26") {
    val input = "(A) -> ((B, C) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function27") {
    val input = "((A) -> (B, C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function28") {
    val input = "(A) -> ((B, C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function29") {
    val input = "(A) -> (((B, C)) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function30") {
    val input = "(A) -> (B) -> (C, D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function31") {
    val input = "(A) -> ((B) -> (C, D))"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function32") {
    val input = "((A) -> (B)) -> (C, D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Tuple])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Tuple].elms.forall(_.isInstanceOf[ParsedAst.Type.Named]))
  }

  test("Type.Function33") {
    val input = "(A) -> (B) -> (C) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function34") {
    val input = "(A) -> ((B) -> (C) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function35") {
    val input = "(A) -> ((B) -> ((C) -> D))"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function36") {
    val input = "(A) -> (((B) -> (C)) -> D)"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function37") {
    val input = "((A) -> (B) -> (C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function38") {
    val input = "((A) -> ((B) -> (C))) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function39") {
    val input = "(((A) -> (B)) -> (C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Function40") {
    val input = "(A) -> ((B) -> (C)) -> D"
    val result = new Parser(SourceInput.Str(input)).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Function])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].formals.head.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].formals.head.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
    assert(result.retTpe.asInstanceOf[ParsedAst.Type.Function].retTpe.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Tuple01") {
    val input = "()"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assertResult(result.get)(ParsedAst.Type.Unit)
  }

  test("Type.Tuple02") {
    val input = "(A)"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Named])
  }

  test("Type.Tuple03") {
    val input = "(A, B)"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Tuple])
    assertResult(2)(result.get.asInstanceOf[ParsedAst.Type.Tuple].elms.length)
  }

  test("Type.Tuple04") {
    val input = "(A, B, C)"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Tuple])
    assertResult(3)(result.get.asInstanceOf[ParsedAst.Type.Tuple].elms.length)
  }

  test("Type.Parametric01") {
    val input = "A[B]"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  test("Type.Parametric02") {
    val input = "A[B, C]"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  test("Type.Parametric03") {
    val input = "A[B, C[D, E]]"
    val result = new Parser(SourceInput.Str(input)).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Ident01") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("x")(result.name)
  }

  test("Ident02") {
    val input = "y"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("y")(result.name)
  }

  test("Ident03") {
    val input = "x0"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("x0")(result.name)
  }

  test("Ident04") {
    val input = "x'"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("x'")(result.name)
  }

  test("Ident05") {
    val input = "foobar"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("foobar")(result.name)
  }

  test("Ident06") {
    val input = "fooBar"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("fooBar")(result.name)
  }

  test("Ident07") {
    val input = "foo_bar"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("foo_bar")(result.name)
  }

  test("Ident08") {
    val input = "f00_BAR'''"
    val result = new Parser(SourceInput.Str(input)).Ident.run().get
    assertResult("f00_BAR'''")(result.name)
  }

  test("Ident09") {
    val input = "1"
    val result = new Parser(SourceInput.Str(input)).Ident.run()
    assert(result.isFailure)
  }

  test("Ident10") {
    val input = "'"
    val result = new Parser(SourceInput.Str(input)).Ident.run()
    assert(result.isFailure)
  }

  test("Ident11") {
    val input = "_"
    val result = new Parser(SourceInput.Str(input)).Ident.run()
    assert(result.isFailure)
  }

  test("QName01") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).QName.run().get
    assertResult(Seq("x"))(result.parts)
  }

  test("QName02") {
    val input = "x::y"
    val result = new Parser(SourceInput.Str(input)).QName.run().get
    assertResult(Seq("x", "y"))(result.parts)
  }

  test("QName03") {
    val input = "x::y::z"
    val result = new Parser(SourceInput.Str(input)).QName.run().get
    assertResult(Seq("x", "y", "z"))(result.parts)
  }

  test("QName04") {
    val input = "abc::def::hij"
    val result = new Parser(SourceInput.Str(input)).QName.run().get
    assertResult(Seq("abc", "def", "hij"))(result.parts)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Should literals also include tuples???
  // TODO: The parser could be simplified by allowing expressions everywhere and
  // then simply checking whether such an expression is a literal.

  test("Literal (Unit)") {
    val input = "()"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Unit])
  }

  test("Literal (true)") {
    val input = "true"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get.asInstanceOf[ParsedAst.Literal.Bool]
    assertResult("true")(result.lit)
  }

  test("Literal (false)") {
    val input = "false"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get.asInstanceOf[ParsedAst.Literal.Bool]
    assertResult("false")(result.lit)
  }

  test("Literal (123)") {
    val input = "123"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get.asInstanceOf[ParsedAst.Literal.Int]
    assertResult("123")(result.lit)
  }

  test("Literal (\"\")") {
    val input = "\"\""
    val result = new Parser(SourceInput.Str(input)).Literal.run().get.asInstanceOf[ParsedAst.Literal.Str]
    assertResult("")(result.lit)
  }

  test("Literal (\"foo\")") {
    val input = "\"foo\""
    val result = new Parser(SourceInput.Str(input)).Literal.run().get.asInstanceOf[ParsedAst.Literal.Str]
    assertResult("foo")(result.lit)
  }

  test("Literal.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Literal.Tag02") {
    val input = "Foo.Bar()"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Literal.Tag03") {
    val input = "Foo.Bar Baz.Quux"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Literal.Tag04") {
    val input = "quux::Foo.Bar(true, 42, \"foo\")"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Literal.Tuple01") {
    val input = "()"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Unit])
  }

  test("Literal.Tuple02") {
    val input = "(1)"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Int])
  }

  test("Literal.Tuple03") {
    val input = "(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tuple])
  }

  test("Literal.Tuple04") {
    val input = "(true, 42, \"foo\")"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tuple])
  }

  test("Literal.Set01") {
    val input = "#{}"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Set])
  }

  test("Literal.Set02") {
    val input = "#{1, 2, 3}"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Set])
  }

  test("Literal.Set03") {
    val input = "#{(1, 2), (2, 3)}"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Set])
  }

  test("Literal.Set04") {
    val input = "#{#{1}, #{2}}"
    val result = new Parser(SourceInput.Str(input)).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Set])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("Operator.Unary !") {
    val input = "!"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.Unary).get
    assertResult(UnaryOperator.LogicalNot)(result)
  }

  test("Operator.Unary +") {
    val input = "+"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.Unary).get
    assertResult(UnaryOperator.Plus)(result)
  }

  test("Operator.Unary -") {
    val input = "-"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.Unary).get
    assertResult(UnaryOperator.Minus)(result)
  }

  test("Operator.Unary ~") {
    val input = "~"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.Unary).get
    assertResult(UnaryOperator.BitwiseNegate)(result)
  }

  test("Operator.Binary.LogicalOp &&") {
    val input = "&&"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.LogicalAnd)(result)
  }

  test("Operator.Binary.LogicalOp ||") {
    val input = "||"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.LogicalOr)(result)
  }

  test("Operator.Binary.LogicalOp ->") {
    val input = "==>"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.Implication)(result)
  }

  test("Operator.Binary.LogicalOp <->") {
    val input = "<==>"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.Biconditional)(result)
  }

  test("Operator.Binary.Bitwise &") {
    val input = "&"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.BitwiseAnd)(result)
  }

  test("Operator.Binary.Bitwise |") {
    val input = "|"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.BitwiseOr)(result)
  }

  test("Operator.Binary.Bitwise ^") {
    val input = "^"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.BitwiseXor)(result)
  }

  test("Operator.Binary.Bitwise <<") {
    val input = "<<"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.BitwiseLeftShift)(result)
  }

  test("Operator.Binary.Bitwise >>") {
    val input = ">>"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.BitwiseRightShift)(result)
  }

  test("Operator.Binary.ComparisonOp <") {
    val input = "<"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.Less)(result)
  }

  test("Operator.Binary.ComparisonOp <=") {
    val input = "<="
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.LessEqual)(result)
  }

  test("Operator.Binary.ComparisonOp >") {
    val input = ">"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.Greater)(result)
  }

  test("Operator.Binary.ComparisonOp >=") {
    val input = ">="
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.GreaterEqual)(result)
  }

  test("Operator.Binary.ComparisonOp ==") {
    val input = "=="
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.Equal)(result)
  }

  test("Operator.Binary.ComparisonOp !=") {
    val input = "!="
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.NotEqual)(result)
  }

  test("Operator.Binary.MultiplicativeOp *") {
    val input = "*"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.MultiplicativeOp).get
    assertResult(BinaryOperator.Times)(result)
  }

  test("Operator.Binary.MultiplicativeOp /") {
    val input = "/"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.MultiplicativeOp).get
    assertResult(BinaryOperator.Divide)(result)
  }

  test("Operator.Binary.MultiplicativeOp %") {
    val input = "%"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.MultiplicativeOp).get
    assertResult(BinaryOperator.Modulo)(result)
  }

  test("Operator.Binary.AdditiveOp +") {
    val input = "+"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.AdditiveOp).get
    assertResult(BinaryOperator.Plus)(result)
  }

  test("Operator.Binary.AdditiveOp -") {
    val input = "-"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.AdditiveOp).get
    assertResult(BinaryOperator.Minus)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // UTF8 Operators                                                          //
  /////////////////////////////////////////////////////////////////////////////
  test("Operator.Unary.UTF8-Negation") {
    val input = "¬"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.Unary).get
    assertResult(UnaryOperator.LogicalNot)(result)
  }

  test("Operator.Binary.UTF8-Equal") {
    val input = "≡"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.ComparisonOp).get
    assertResult(BinaryOperator.Equal)(result)
  }

  test("Operator.Binary.UTF8-Conjunction") {
    val input = "∧"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.LogicalAnd)(result)
  }

  test("Operator.Binary.UTF8-Disjunction") {
    val input = "∨"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.LogicalOr)(result)
  }

  test("Operator.Binary.UTF8-Implication") {
    val input = "→"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.Implication)(result)
  }

  test("Operator.Binary.UTF8-Biconditional") {
    val input = "↔"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operator.LogicalOp).get
    assertResult(BinaryOperator.Biconditional)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Annotations                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Annotation @strict") {
    val input = "@strict"
    val parser = mkParser(input)
    val result = parser.__run(parser.Annotation).get
    assert(result.isInstanceOf[ParsedAst.Annotation])
  }

  test("Annotation @monotone") {
    val input = "@monotone"
    val parser = mkParser(input)
    val result = parser.__run(parser.Annotation).get
    assert(result.isInstanceOf[ParsedAst.Annotation])
  }

  test("Annotation.AnnotatedFunction01") {
    val input =
      """@strict
        |fn f(x: Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition)
    assert(result.get.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedFunction02") {
    val input =
      """@monotone
        |fn f(x: Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedFunction03") {
    val input =
      """@strict @monotone
        |fn f(x: Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedFunction04") {
    val input =
      """@strict @monotone @commutative @associative @unsafe @unchecked
        |fn f(x: Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedParameter01") {
    val input =
      """fn f(x: @strict Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedParameter02") {
    val input =
      """fn f(x: @monotone Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedParameter03") {
    val input =
      """fn f(x: @strict @monotone Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Annotation.AnnotatedParameter04") {
    val input =
      """fn f(x: @strict Int, y: Int, z: @monotone Int): Int = x
      """.stripMargin
    val parser = mkParser(input)
    val result = parser.__run(parser.FunctionDefinition).get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("WhiteSpace (1)") {
    val input = " "
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  test("WhiteSpace (2)") {
    val input = "    "
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  test("WhiteSpace (3)") {
    val input = "\t"
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  test("WhiteSpace (4)") {
    val input = "\n\r"
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  test("WhiteSpace (5)") {
    val input = " // comments are also whitespace "
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  test("WhiteSpace (6)") {
    val input = " /* comments are also whitespace */ "
    val result = new Parser(SourceInput.Str(input)).WS.run()
    assert(result.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("SingleLineComment (1)") {
    val input = "// a comment"
    val result = new Parser(SourceInput.Str(input)).SingleLineComment.run()
    assert(result.isSuccess)
  }

  test("SingleLineComment (2)") {
    val input =
      """// a comment
        |// another comment
        |// and yet another
      """.stripMargin
    val result = new Parser(SourceInput.Str(input)).SingleLineComment.run()
    assert(result.isSuccess)
  }

  test("MultiLineComment (1)") {
    val input = "/* a comment */"
    val result = new Parser(SourceInput.Str(input)).MultiLineComment.run()
    assert(result.isSuccess)
  }

  test("MultiLineComment (2)") {
    val input =
      """/*
        |a comment
        |*/""".stripMargin
    val result = new Parser(SourceInput.Str(input)).MultiLineComment.run()
    assert(result.isSuccess)
  }


  /**
    * Returns a parser for the given string `s`.
    */
  private def mkParser(s: String): Parser = new Parser(SourceInput.Str(s))
}
