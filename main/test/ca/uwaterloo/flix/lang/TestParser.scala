package ca.uwaterloo.flix.lang

import ca.uwaterloo.flix.lang.ast.{Ast, BinaryOperator, UnaryOperator}

import org.scalatest.FunSuite

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Namespace01") {
    val input =
      """namespace foo {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(None, input).NamespaceDeclaration.run().get
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Parser.Namespace02") {
    val input =
      """namespace foo::bar::baz {
        |  // a comment
        |}
      """.stripMargin
    val result = new Parser(None, input).NamespaceDeclaration.run().get
    assertResult(Seq("foo", "bar", "baz"))(result.name.parts)
  }

  test("Parser.Namespace03") {
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

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Term01") {
    val input = "_"
    val result = new Parser(None, input).Term.run().get
    assert(result.isInstanceOf[Ast.Term.Wildcard])
  }

  test("Parser.Term02") {
    val input = "x"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[Ast.Term.Var]
    assertResult("x")(result.ident.name)
  }

  test("Parser.Term03") {
    val input = "42"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[Ast.Term.Lit]
    assertResult(42)(result.literal.asInstanceOf[Ast.Literal.Int].literal)
  }

  test("Parser.Term04") {
    val input = "foo(x)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[Ast.Term.Apply]
    assertResult(Seq("foo"))(result.name.parts)
  }

  test("Parser.Term05") {
    val input = "foo::bar(x, y, z)"
    val result = new Parser(None, input).Term.run().get.asInstanceOf[Ast.Term.Apply]
    assertResult(Seq("foo", "bar"))(result.name.parts)
    assertResult(Seq("x", "y", "z"))(result.arguments.map(_.asInstanceOf[Ast.Term.Var].ident.name))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Type.List01") {
    val input = "List[A]"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[Ast.Type.List]
    assertResult(Seq("A"))(result.elms.asInstanceOf[Ast.Type.Ambiguous].name.parts)
  }

  test("Parser.Type.Set01") {
    val input = "Set[A]"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[Ast.Type.Set]
    assertResult(Seq("A"))(result.elms.asInstanceOf[Ast.Type.Ambiguous].name.parts)
  }

  test("Parser.Type.Map01") {
    val input = "Map[A, B]"
    val result = new Parser(None, input).Type.run().get.asInstanceOf[Ast.Type.Map]
    assertResult(Seq("A"))(result.t1.asInstanceOf[Ast.Type.Ambiguous].name.parts)
    assertResult(Seq("B"))(result.t2.asInstanceOf[Ast.Type.Ambiguous].name.parts)
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
  test("Parser.Literal (Unit)") {
    val input = "()"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Unit)
  }

  test("Parser.Literal (true)") {
    val input = "true"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Bool(literal = true))
  }

  test("Parser.Literal (false)") {
    val input = "false"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Bool(literal = false))
  }

  test("Parser.Literal (123)") {
    val input = "123"
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Int(literal = 123))
  }

  test("Parser.Literal (\"\")") {
    val input = "\"\""
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Str(literal = ""))
  }

  test("Parser.Literal (\"foo\")") {
    val input = "\"foo\""
    val result = new Parser(None, input).Literal.run().get
    assertResult(result)(Ast.Literal.Str(literal = "foo"))
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
