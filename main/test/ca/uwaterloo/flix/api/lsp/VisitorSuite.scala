package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.provider.Visitor
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition}
import ca.uwaterloo.flix.language.ast.shared.{Input, Source, SecurityContext}

import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertions


class VisitorSuite extends AnyFunSuite {
  val source = Source(Input.Text("test", "test", true, SecurityContext.AllPermissions), Array.emptyCharArray)
  val uri = "test"
   
  test("Inside.01") {
    val loc = SourceLocation(
      false, 
      SourcePosition(source, 3, 10), 
      SourcePosition(source, 6, 2))
    val pos = Position.fromZeroIndexed(4, 4)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.02") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 5, 5),
    )
    val pos = Position.fromZeroIndexed(4, 3)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.03") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 5, 4),
      SourcePosition(source, 5, 5),
    )
    val pos = Position.fromZeroIndexed(4, 3)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.04") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 5),
    )
    val pos = Position.fromZeroIndexed(5, 3)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.05") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 4, 4),
      SourcePosition(source, 6, 10),
    )
    val pos = Position.fromZeroIndexed(2, 6)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.06") {
    
    val loc = SourceLocation(
      false,
      SourcePosition(source, 3, 2),
      SourcePosition(source, 6, 5),
    )
    val pos = Position.fromZeroIndexed(6, 3)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.07") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position.fromZeroIndexed(5, 10)

    assert(Visitor.inside(uri, pos)(loc) === false)
    
  }

  test("Inside.08") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position.fromZeroIndexed(1, 2)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.09") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 6),
    )
    val pos = Position.fromZeroIndexed(5, 1)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }
}
