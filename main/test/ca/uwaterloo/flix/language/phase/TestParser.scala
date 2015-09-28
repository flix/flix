package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import org.scalatest.FunSuite

// TODO: Cleanup names. Numbering and remove the Parser. prefix.
// TODO: Write custom assert which will actually print the parse error...

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Root01") {
    val input = ""
    val result = new Parser(None, input).Root.run().get
    assert(result.isInstanceOf[ParsedAst.Root])
  }

  test("Parser.Root02") {
    val input =
      """namespace a {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(None, input).Root.run().get
    assert(result.isInstanceOf[ParsedAst.Root])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations and Definitions                                            //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Declaration.Namespace01") {
    val input =
      """namespace foo {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(None, input).NamespaceDeclaration.run().get
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Parser.Declaration.Namespace02") {
    val input =
      """namespace foo::bar::baz {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(None, input).NamespaceDeclaration.run().get
    assertResult(Seq("foo", "bar", "baz"))(result.name.parts)
  }

  test("Parser.Declaration.Namespace03") {
    val input =
      """namespace foo {
        |  namespace bar {
        |    namespace baz {
        |      // a comment
        |    }
        |  }
        |}
      """.stripMargin
    val result = new Parser(None, input).NamespaceDeclaration.run()
    assert(result.isSuccess)
  }

  test("Parser.Definition.Value01") {
    val input = "val v: Int = 42;"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Value])
  }

  test("Parser.Definition.Value02") {
    val input = "val v: Int = 1 + 1;"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Value])
  }

  test("Parser.Definition.Function01") {
    val input = "def foo(x: Int): Int = 42"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Parser.Definition.Function02") {
    val input = "def foo(x: Int, y: Int, z: Int): Int = x + y + z"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Function])
  }

  test("Parser.Definition.Enum01") {
    val input =
      """enum A {
        |  case B
        |}
      """.stripMargin
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  test("Parser.Definition.Enum02") {
    val input =
      """enum A {
        |  case B(Int)
        |}
      """.stripMargin
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  test("Parser.Definition.Enum03") {
    val input =
      """enum A {
        |  case B,
        |  case C(Int),
        |  case D(Bool, Int, Str)
        |}
      """.stripMargin
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Enum])
  }

  // TODO: Allow naming of the enum attributes.

  test("Parser.Definition.JoinSemiLattice01") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice02") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice03") {
    val input = "lat <a> (Tag.Bot, fn (x: Int): Bool = x, lub)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice04") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub) with Norm(b)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice05") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub) with Widen(b)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice06") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub) with Norm(b) with Widen(c)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.JoinSemiLattice07") {
    val input = "lat <a> (Tag.Bot, foo::leq, lub) with Norm(foo::b) with Widen(foo::c)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  ignore("Parser.Definition.CompleteLattice01") {
    val input = "lat <a> (Tag.Bot, top, foo::leq, lub, glb)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  ignore("Parser.Definition.CompleteLattice02") {
    val input = "lat <a> (Tag.Bot, top, foo::leq, lub, glb) with Norm(b)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  ignore("Parser.Definition.CompleteLattice03") {
    val input = "lat <a> (Tag.Bot, top, foo::leq, lub, glb) with Widen(b)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  ignore("Parser.Definition.CompleteLattice04") {
    val input = "lat <a> (Tag.Bot, top, foo::leq, lub, glb) with Norm(b) with Widen(c)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  ignore("Parser.Definition.CompleteLattice05") {
    val input = "lat <a> (Tag.Bot, top, foo::leq, lub, glb) with Norm(foo::b) with Widen(foo::c)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Lattice])
  }

  test("Parser.Definition.Relation01") {
    val input = "rel A(b: B)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Relation])
  }

  test("Parser.Definition.Relation02") {
    val input = "rel A(b: B, c: C, d: D)"
    val result = new Parser(None, input).Definition.run().get
    assert(result.isInstanceOf[ParsedAst.Definition.Relation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Check up on associativity.

  test("Parser.Expression.LogicalExp01") {
    val input = "true && false"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.And)(result.op)
  }

  test("Parser.Expression.LogicalExp02") {
    val input = "true || false"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Or)(result.op)
  }

  test("Parser.Expression.LogicalExp03") {
    val input = "1 < 2 && 3 < 4"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.And)(result.op)
  }

  test("Parser.Expression.ComparisonExp01") {
    val input = "1 < 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Less)(result.op)
  }

  test("Parser.Expression.ComparisonExp02") {
    val input = "1 + 2 > 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Greater)(result.op)
  }

  test("Parser.Expression.ComparisonExp03") {
    val input = "1 + 2 > 3 + 4"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Greater)(result.op)
  }

  test("Parser.Expression.MultiplicativeExp01") {
    val input = "1 * 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Times)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Parser.Expression.MultiplicativeExp02") {
    val input = "1 * 2 * 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Times)(result.op)
  }

  test("Parser.Expression.MultiplicativeExp03") {
    val input = "1 * 2 + 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Parser.Expression.MultiplicativeExp04") {
    val input = "1 + 2 * 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Parser.Expression.AdditiveExp01") {
    val input = "1 + 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Parser.Expression.AdditiveExp02") {
    val input = "1 + 2 + 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Plus)(result.op)
  }

  test("Parser.Expression.AdditiveExp03") {
    val input = "1 - 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(result.op)
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Parser.Expression.AdditiveExp04") {
    val input = "1 - 2 - 3"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(result.op)
    val e1 = result.e1.asInstanceOf[ParsedAst.Expression.Binary]
    assertResult(BinaryOperator.Minus)(e1.op)
    assert(e1.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(e1.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Parser.Expression.AdditiveExp05") {
    val input = "1 + 2 - 3 + 4 - 5 + 6"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
  }

  test("Parser.Expression.Infix01") {
    val input = "1 `plus` 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assertResult(Seq("plus"))(result.name.parts)
  }

  test("Parser.Expression.Infix02") {
    val input = "1 `foo::bar::baz::plus` 2"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Lit])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Lit])
    assertResult(Seq("foo", "bar", "baz", "plus"))(result.name.parts)
  }

  test("Parser.Expression.Infix03") {
    val input = "+1 `plus` -1"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Infix]
    assert(result.e1.isInstanceOf[ParsedAst.Expression.Unary])
    assert(result.e2.isInstanceOf[ParsedAst.Expression.Unary])
  }

  test("Parser.Expression.UnaryExp01") {
    val input = "+ 1"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.UnaryPlus)(result.op)
  }

  test("Parser.Expression.UnaryExp02") {
    val input = "- 1"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.UnaryMinus)(result.op)
  }

  test("Parser.Expression.UnaryExp03") {
    val input = "!! true"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Unary]
    assertResult(UnaryOperator.Not)(result.op)
  }

  test("Parser.Expression.Ascribe01") {
    val input = "true: Bool"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Ascribe])
  }

  test("Parser.Expression.Ascribe02") {
    val input = "x: Bool -> Int"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Ascribe])
  }

  test("Parser.Expression.LiteralExp01") {
    val input = "true"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(true)(result.literal.asInstanceOf[ParsedAst.Literal.Bool].literal)
  }

  test("Parser.Expression.LiteralExp02") {
    val input = "42"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(42)(result.literal.asInstanceOf[ParsedAst.Literal.Int].literal)
  }

  test("Parser.Expression.LiteralExp03") {
    val input = "\"foo\""
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult("foo")(result.literal.asInstanceOf[ParsedAst.Literal.Str].literal)
  }

  test("Parser.Expression.LetExp01") {
    val input = "let x = 42 in x"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("x")(result.ident.name)
  }

  test("Parser.Expression.LetExp02") {
    val input = "let x' = f(1, 2, 3) in g(4, 5, 6)"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("x'")(result.ident.name)
  }

  test("Parser.Expression.LetExp03") {
    val input =
      """let x = 1 in
        |let y = 2 in
        |let z = 3 in
        |  42""".stripMargin
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    val l1 = result.get.asInstanceOf[ParsedAst.Expression.Let]
    val l2 = l1.body.asInstanceOf[ParsedAst.Expression.Let]
    val l3 = l2.body.asInstanceOf[ParsedAst.Expression.Let]
    assertResult("z")(l3.ident.name)
  }

  test("Parser.Expression.IfThenElseExp01") {
    val input = "if (1) 2 else 3"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Parser.Expression.IfThenElseExp02") {
    val input = "if (f(1, 2, 3)) g(4, 5, 6) else h(7, 8, 9)"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Parser.Expression.IfThenElseExp03") {
    val input = "if ((1)) (2) else (3)"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.IfThenElse])
  }

  test("Parser.Expression.MatchExp01") {
    val input =
      """match 1 with {
        |  case 2 => 3
        |}
      """.stripMargin
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Match])
  }

  test("Parser.Expression.MatchExp02") {
    val input =
      """match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |}
      """.stripMargin
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Match])
  }

  test("Parser.Expression.MatchExp03") {
    val input =
      """match 1 with {
        |  case 2 => match 3 with {
        |    case 4 => 5
        |  }
        |  case 6 => 7
        |}
      """.stripMargin
    val result = new Parser(None, input).Expression.run().get
    val m1 = result.asInstanceOf[ParsedAst.Expression.Match]
    val m2 = m1.rules.head._2.asInstanceOf[ParsedAst.Expression.Match]
    val l = m2.rules.head._2.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(5)(l.literal.asInstanceOf[ParsedAst.Literal.Int].literal)
  }

  test("Parser.Expression.MatchExp04") {
    val input =
      """match
        |  match 1 with {
        |    case 2 => 3
        |  } with {
        |    case 4 => 5
        |}
      """.stripMargin
    val result = new Parser(None, input).Expression.run().get
    val m1 = result.asInstanceOf[ParsedAst.Expression.Match]
    val m2 = m1.e.asInstanceOf[ParsedAst.Expression.Match]
    val l = m2.rules.head._2.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(3)(l.literal.asInstanceOf[ParsedAst.Literal.Int].literal)
  }

  test("Parser.Expression.CallExp01") {
    val input = "f()"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousApply])
  }

  test("Parser.Expression.CallExp02") {
    val input = "f(1, 2, 3)"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousApply])
  }

  test("Parser.Expression.CallExp03") {
    val input = "f(f(1), f(f(2)))"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousApply])
  }

  test("Parser.Expression.CallExp04") {
    val input = "foo::bar::baz::f(1, 2, 3)"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousApply])
  }

  test("Parser.Expression.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Parser.Expression.Tag02") {
    val input = "Foo.Bar ()"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Parser.Expression.Tag03") {
    val input = "Foo.Bar Baz.Qux"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Parser.Expression.Tag04") {
    val input = "Foo.Bar (x, y)"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Parser.Expression.Tag05") {
    val input = "foo::bar::Baz.Qux (42, x, (3, 4))"
    val result = new Parser(None, input).Expression.run().get
    assert(result.isInstanceOf[ParsedAst.Expression.Tag])
  }

  test("Parser.Expression.Tuple01") {
    val input = "()"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(ParsedAst.Literal.Unit)(result.literal)
  }

  test("Parser.Expression.Tuple02") {
    val input = "(1)"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lit]
    assertResult(ParsedAst.Literal.Int(1))(result.literal)
  }

  test("Parser.Expression.Tuple03") {
    val input = "(1, x)"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(2)(result.elms.size)
  }

  test("Parser.Expression.Tuple04") {
    val input = "(1, 2, x, 4, 5, 6)"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(6)(result.elms.size)
  }

  test("Parser.Expression.Tuple05") {
    val input = "((1, 2), (x, (4, 5)))"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Tuple]
    assertResult(2)(result.elms.size)
  }

  test("Parser.Expression.Var01") {
    val input = "x"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousVar])
  }

  test("Parser.Expression.Var02") {
    val input = "foo::bar::baz::x_y_z''"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.AmbiguousVar])
  }

  test("Parser.Expression.Lambda01") {
    val input = "fn(x: Int): Int = 42"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lambda]
    assert(result.body.isInstanceOf[ParsedAst.Expression.Lit])
  }

  test("Parser.Expression.Lambda02") {
    val input = "fn(x: Bool, y: Int, z: Str): Str = x + y + z"
    val result = new Parser(None, input).Expression.run().get.asInstanceOf[ParsedAst.Expression.Lambda]
    assert(result.body.isInstanceOf[ParsedAst.Expression.Binary])
  }

  test("Parser.Expression.ErrorExp01") {
    val input = "???"
    val result = new Parser(None, input).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Error])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Pattern.Wildcard") {
    val input = "_"
    val result = new Parser(None, input).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Wildcard])
  }

  test("Parser.Pattern.Var01") {
    val input = "x"
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Var]
    assertResult("x")(result.ident.name)
  }

  test("Parser.Pattern.Var02") {
    val input = "foo_Bar'"
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Var]
    assertResult("foo_Bar'")(result.ident.name)
  }

  test("Parser.Pattern.Literal01") {
    val input = "true"
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult(ParsedAst.Literal.Bool(true))(result.literal)
  }

  test("Parser.Pattern.Literal02") {
    val input = "42"
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult(ParsedAst.Literal.Int(42))(result.literal)
  }

  test("Parser.Pattern.Literal03") {
    val input = "\"foo\""
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult(ParsedAst.Literal.Str("foo"))(result.literal)
  }

  test("Parser.Pattern.Tag01") {
    val input = "Const.Bot"
    val result = new Parser(None, input).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Parser.Pattern.Tag02") {
    val input = "Const.Cst(5)"
    val result = new Parser(None, input).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Parser.Pattern.Tag03") {
    val input = "Foo.Bar (x, _, z)"
    val result = new Parser(None, input).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Parser.Pattern.Tag04") {
    val input = "foo::bar::baz.Foo(x, y, z)"
    val result = new Parser(None, input).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Parser.Pattern.Tuple01") {
    val input = "(x, y, true)"
    val result = new Parser(None, input).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Tuple]
    assertResult(3)(result.elms.size)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.FactDeclaration01") {
    val input = "P(42)."
    val result = new Parser(None, input).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("Parser.FactDeclaration02") {
    val input = "P(\"foo\")."
    val result = new Parser(None, input).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("Parser.FactDeclaration03") {
    val input = "P(f(1, 2, 3))."
    val result = new Parser(None, input).FactDeclaration.run()
    assert(result.isSuccess)
  }

  test("Parser.RuleDeclaration01") {
    val input = "P(x) :- A(x)."
    val result = new Parser(None, input).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(1)(result.get.body.size)
  }

  test("Parser.RuleDeclaration02") {
    val input = "P(x, y, z) :- A(x), B(y), C(z)."
    val result = new Parser(None, input).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(3)(result.get.body.size)
  }

  test("Parser.RuleDeclaration03") {
    val input = "P(f(x), g(y, z)) :- isFoo(x, y), isBar(y, z), A(x), B(y), C(z)."
    val result = new Parser(None, input).RuleDeclaration.run()
    assert(result.isSuccess)
    assertResult(5)(result.get.body.size)
  }

  // TODO: TestPredicate

  // TODO
  test("Parser.Predicate.Alias01") {
    val input = "r := 42"
    val result = new Parser(None, input).Predicate.run()
  }

  test("Parser.Predicate.Alias02") {
    val input = "r := (true, 42, \"foo\")"
    val result = new Parser(None, input).Predicate.run()
  }

  test("Parser.Predicate.Alias03") {
    val input = "r := f(x, g(y, z))"
    val result = new Parser(None, input).Predicate.run()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Term01") {
    val input = "_"
    val result = new Parser(None, input).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Wildcard])
  }

  test("Parser.Term02") {
    val input = "x"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Var]
    assertResult("x")(result.ident.name)
  }

  test("Parser.Term03") {
    val input = "42"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assertResult(42)(result.literal.asInstanceOf[ParsedAst.Literal.Int].literal)
  }

  test("Parser.Term04") {
    val input = "foo(x)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Parser.Term05") {
    val input = "foo::bar(x, y, z)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    assertResult(Seq("foo", "bar"))(result.name.parts)
    assertResult(Seq("x", "y", "z"))(result.args.map(_.asInstanceOf[ParsedAst.Term.Var].ident.name))
  }

  test("Parser.Term.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.literal.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Term.Tag02") {
    val input = "Foo.Bar(1)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.literal.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Term.Tag03") {
    val input = "foo::bar::Baz.Bar(1, 2, 3)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assert(result.literal.isInstanceOf[ParsedAst.Literal.Tag])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Type.Function01") {
    val input = "A -> B"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assertResult(Seq("A"))(result.t1.asInstanceOf[ParsedAst.Type.Ambiguous].name.parts)
    assertResult(Seq("B"))(result.t2.asInstanceOf[ParsedAst.Type.Ambiguous].name.parts)
  }

  test("Parser.Type.Function02") {
    val input = "A -> B -> C"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.t1.isInstanceOf[ParsedAst.Type.Ambiguous])
    assert(result.t2.isInstanceOf[ParsedAst.Type.Function])
  }

  test("Parser.Type.Function03") {
    val input = "(A -> B) -> C"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[ParsedAst.Type.Function]
    assert(result.t1.isInstanceOf[ParsedAst.Type.Function])
    assert(result.t2.isInstanceOf[ParsedAst.Type.Ambiguous])
  }

  test("Parser.Type.Tuple01") {
    val input = "()"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assertResult(result.get)(ParsedAst.Type.Unit)
  }

  test("Parser.Type.Tuple02") {
    val input = "(A)"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Ambiguous])
  }

  test("Parser.Type.Tuple03") {
    val input = "(A, B)"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Tuple])
    assertResult(2)(result.get.asInstanceOf[ParsedAst.Type.Tuple].elms.length)
  }

  test("Parser.Type.Tuple04") {
    val input = "(A, B, C)"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Tuple])
    assertResult(3)(result.get.asInstanceOf[ParsedAst.Type.Tuple].elms.length)
  }

  test("Parser.Type.Parametric01") {
    val input = "A[B]"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  test("Parser.Type.Parametric02") {
    val input = "A[B, C]"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  test("Parser.Type.Parametric03") {
    val input = "A[B, C[D, E]]"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Parametric])
  }

  test("Parser.Type.Lattice01") {
    val input = "<Sign>"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Lattice])
  }

  test("Parser.Type.Lattice02") {
    val input = "<foo::bar::Sign>"
    val result = new Parser(None, input).Type.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Type.Lattice])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Ident01") {
    val input = "x"
    val result = new Parser(None, input).Ident.run().get
    assertResult("x")(result.name)
  }

  test("Parser.Ident02") {
    val input = "y"
    val result = new Parser(None, input).Ident.run().get
    assertResult("y")(result.name)
  }

  test("Parser.Ident03") {
    val input = "x0"
    val result = new Parser(None, input).Ident.run().get
    assertResult("x0")(result.name)
  }

  test("Parser.Ident04") {
    val input = "x'"
    val result = new Parser(None, input).Ident.run().get
    assertResult("x'")(result.name)
  }

  test("Parser.Ident05") {
    val input = "foobar"
    val result = new Parser(None, input).Ident.run().get
    assertResult("foobar")(result.name)
  }

  test("Parser.Ident06") {
    val input = "fooBar"
    val result = new Parser(None, input).Ident.run().get
    assertResult("fooBar")(result.name)
  }

  test("Parser.Ident07") {
    val input = "foo_bar"
    val result = new Parser(None, input).Ident.run().get
    assertResult("foo_bar")(result.name)
  }

  test("Parser.Ident08") {
    val input = "f00_BAR'''"
    val result = new Parser(None, input).Ident.run().get
    assertResult("f00_BAR'''")(result.name)
  }

  test("Parser.Ident09") {
    val input = "1"
    val result = new Parser(None, input).Ident.run()
    assert(result.isFailure)
  }

  test("Parser.Ident10") {
    val input = "'"
    val result = new Parser(None, input).Ident.run()
    assert(result.isFailure)
  }

  test("Parser.Ident11") {
    val input = "_"
    val result = new Parser(None, input).Ident.run()
    assert(result.isFailure)
  }

  test("Parser.QName01") {
    val input = "x"
    val result = new Parser(None, input).QName.run().get
    assertResult(Seq("x"))(result.parts)
  }

  test("Parser.QName02") {
    val input = "x::y"
    val result = new Parser(None, input).QName.run().get
    assertResult(Seq("x", "y"))(result.parts)
  }

  test("Parser.QName03") {
    val input = "x::y::z"
    val result = new Parser(None, input).QName.run().get
    assertResult(Seq("x", "y", "z"))(result.parts)
  }

  test("Parser.QName04") {
    val input = "abc::def::hij"
    val result = new Parser(None, input).QName.run().get
    assertResult(Seq("abc", "def", "hij"))(result.parts)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  // TODO: Should literals also include tuples???
  // TODO: The parser could be simplified by allowing expressions everywhere and
  // then simply checking whether such an expression is a literal.

  test("Parser.Literal (Unit)") {
    val input = "()"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Unit)
  }

  test("Parser.Literal (true)") {
    val input = "true"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Bool(literal = true))
  }

  test("Parser.Literal (false)") {
    val input = "false"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Bool(literal = false))
  }

  test("Parser.Literal (123)") {
    val input = "123"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Int(literal = 123))
  }

  test("Parser.Literal (\"\")") {
    val input = "\"\""
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Str(literal = ""))
  }

  test("Parser.Literal (\"foo\")") {
    val input = "\"foo\""
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(ParsedAst.Literal.Str(literal = "foo"))
  }

  test("Parser.Literal.Tag01") {
    val input = "Foo.Bar"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Literal.Tag02") {
    val input = "Foo.Bar()"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Literal.Tag03") {
    val input = "Foo.Bar Baz.Quux"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Literal.Tag04") {
    val input = "quux::Foo.Bar(true, 42, \"foo\")"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tag])
  }

  test("Parser.Literal.Tuple01") {
    val input = "()"
    val result = new Parser(None, input).Literal.run().get
    assertResult(ParsedAst.Literal.Unit)(result)
  }

  test("Parser.Literal.Tuple02") {
    val input = "(1)"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Int])
  }

  test("Parser.Literal.Tuple03") {
    val input = "(1, 2, 3)"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tuple])
  }

  test("Parser.Literal.Tuple04") {
    val input = "(true, 42, \"foo\")"
    val result = new Parser(None, input).Literal.run().get
    assert(result.isInstanceOf[ParsedAst.Literal.Tuple])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.UnaryOp !") {
    val input = "!"
    val result = new Parser(None, input).UnaryOp.run().get
    assertResult(UnaryOperator.Not)(result)
  }

  test("Parser.UnaryOp +") {
    val input = "+"
    val result = new Parser(None, input).UnaryOp.run().get
    assertResult(UnaryOperator.UnaryPlus)(result)
  }

  test("Parser.UnaryOp -") {
    val input = "-"
    val result = new Parser(None, input).UnaryOp.run().get
    assertResult(UnaryOperator.UnaryMinus)(result)
  }

  test("Parser.LogicalOp &&") {
    val input = "&&"
    val result = new Parser(None, input).LogicalOp.run().get
    assertResult(BinaryOperator.And)(result)
  }

  test("Parser.LogicalOp ||") {
    val input = "||"
    val result = new Parser(None, input).LogicalOp.run().get
    assertResult(BinaryOperator.Or)(result)
  }

  test("Parser.ComparisonOp <") {
    val input = "<"
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.Less)(result)
  }

  test("Parser.ComparisonOp <=") {
    val input = "<="
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.LessEqual)(result)
  }

  test("Parser.ComparisonOp >") {
    val input = ">"
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.Greater)(result)
  }

  test("Parser.ComparisonOp >=") {
    val input = ">="
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.GreaterEqual)(result)
  }

  test("Parser.ComparisonOp ==") {
    val input = "=="
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.Equal)(result)
  }

  test("Parser.ComparisonOp !=") {
    val input = "!="
    val result = new Parser(None, input).ComparisonOp.run().get
    assertResult(BinaryOperator.NotEqual)(result)
  }

  test("Parser.MultiplicativeOp *") {
    val input = "*"
    val result = new Parser(None, input).MultiplicativeOp.run().get
    assertResult(BinaryOperator.Times)(result)
  }

  test("Parser.MultiplicativeOp /") {
    val input = "/"
    val result = new Parser(None, input).MultiplicativeOp.run().get
    assertResult(BinaryOperator.Divide)(result)
  }

  test("Parser.MultiplicativeOp %") {
    val input = "%"
    val result = new Parser(None, input).MultiplicativeOp.run().get
    assertResult(BinaryOperator.Modulo)(result)
  }

  test("Parser.AdditiveOp +") {
    val input = "+"
    val result = new Parser(None, input).AdditiveOp.run().get
    assertResult(BinaryOperator.Plus)(result)
  }

  test("Parser.AdditiveOp -") {
    val input = "-"
    val result = new Parser(None, input).AdditiveOp.run().get
    assertResult(BinaryOperator.Minus)(result)
  }

  // TODO: Shift operators

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.WhiteSpace (1)") {
    val input = " "
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (2)") {
    val input = "    "
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (3)") {
    val input = "\t"
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (4)") {
    val input = "\n\r"
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (5)") {
    val input = " // comments are also whitespace "
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (6)") {
    val input = " /* comments are also whitespace */ "
    val result = new Parser(None, input).WS.run()
    assert(result.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.SingleLineComment (1)") {
    val input = "// a comment"
    val result = new Parser(None, input).SingleLineComment.run()
    assert(result.isSuccess)
  }

  test("Parser.SingleLineComment (2)") {
    val input =
      """// a comment
        |// another comment
        |// and yet another
      """.stripMargin
    val result = new Parser(None, input).SingleLineComment.run()
    assert(result.isSuccess)
  }

  test("Parser.MultiLineComment (1)") {
    val input = "/* a comment */"
    val result = new Parser(None, input).MultiLineComment.run()
    assert(result.isSuccess)
  }

  test("Parser.MultiLineComment (2)") {
    val input =
      """/*
        |a comment
        |*/""".stripMargin
    val result = new Parser(None, input).MultiLineComment.run()
    assert(result.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Source Location                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.SourceLocation01") {
    val input = "x"
    val result = new Parser(None, input).Ident.run().get
    assertResult(result.location)(SourceLocation(None, 1, 1))
  }

  // TODO: Add more tests for source location

}
