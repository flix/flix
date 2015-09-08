package ca.uwaterloo.flix.lang

import org.scalatest.FunSuite

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.Ident01") {
    val input = "x"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("x")(result.name)
  }

  test("Parser.Ident02") {
    val input = "y"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("y")(result.name)
  }

  test("Parser.Ident03") {
    val input = "x0"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("x0")(result.name)
  }

  test("Parser.Ident04") {
    val input = "x'"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("x'")(result.name)
  }

  test("Parser.Ident05") {
    val input = "foobar"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("foobar")(result.name)
  }

  test("Parser.Ident06") {
    val input = "fooBar"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("fooBar")(result.name)
  }

  test("Parser.Ident07") {
    val input = "foo_bar"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("foo_bar")(result.name)
  }

  test("Parser.Ident08") {
    val input = "f00_BAR'''"
    val result = new Parser(None, input).Ident2.run().get
    assertResult("f00_BAR'''")(result.name)
  }

  test("Parser.Ident09") {
    val input = "1"
    val result = new Parser(None, input).Ident2.run().isFailure
    assert(result)
  }

  test("Parser.Ident10") {
    val input = "'"
    val result = new Parser(None, input).Ident2.run().isFailure
    assert(result)
  }

  test("Parser.Ident11") {
    val input = "_"
    val result = new Parser(None, input).Ident2.run().isFailure
    assert(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Source Location                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("Parser.SourceLocation01") {
    val input = "x"
    val result = new Parser(None, input).Ident2.run().get
    assertResult(result.location)(SourceLocation(None, 1, 1))
  }

  // TODO: Add more tests for source location

}
