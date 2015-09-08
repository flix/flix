package ca.uwaterloo.flix.lang

import ca.uwaterloo.flix.lang.ast.Ast
import org.scalatest.FunSuite

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers                                                             //
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
    val result = new Parser(None, input).Ident.run().isFailure
    assert(result)
  }

  test("Parser.Ident10") {
    val input = "'"
    val result = new Parser(None, input).Ident.run().isFailure
    assert(result)
  }

  test("Parser.Ident11") {
    val input = "_"
    val result = new Parser(None, input).Ident.run().isFailure
    assert(result)
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
  // Source Location                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.SourceLocation01") {
    val input = "x"
    val result = new Parser(None, input).Ident.run().get
    assertResult(result.location)(SourceLocation(None, 1, 1))
  }

  // TODO: Add more tests for source location

}
