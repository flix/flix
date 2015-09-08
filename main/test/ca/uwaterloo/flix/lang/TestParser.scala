package ca.uwaterloo.flix.lang

import ca.uwaterloo.flix.lang.ast.{Ast, BinaryOperator, UnaryOperator}

import org.scalatest.FunSuite

class TestParser extends FunSuite {

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
    val result = new Parser(None, input).WhiteSpace.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (2)") {
    val input = "    "
    val result = new Parser(None, input).WhiteSpace.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (3)") {
    val input = "\t"
    val result = new Parser(None, input).WhiteSpace.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (4)") {
    val input = "\n\r"
    val result = new Parser(None, input).WhiteSpace.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (5)") {
    val input = " // comments are also whitespace "
    val result = new Parser(None, input).WhiteSpace.run()
    assert(result.isSuccess)
  }

  test("Parser.WhiteSpace (6)") {
    val input = " /* comments are also whitespace */ "
    val result = new Parser(None, input).WhiteSpace.run()
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
