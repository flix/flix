package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourceLocationSuite extends AnyFunSuite {
  test("l1 contains l2 when l1 starts on earlier line and ends on later") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 9),
      SourcePosition(source, 10, 3)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 4, 0),
      SourcePosition(source, 9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start on same line and earlier col and l1 ends on later line and earlier col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 4, 3),
      SourcePosition(source, 10, 3)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 4, 4),
      SourcePosition(source, 9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when it starts on earlier line and later col and ends on same line and later col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 11)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 4, 4),
      SourcePosition(source, 9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and starts on earlier col and ends on later col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 2, 12),
      SourcePosition(source, 9, 2)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and starts on earlier col and ends on same col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 2, 12),
      SourcePosition(source, 9, 4)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and starts on same col and ends on later col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 2)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains itself") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )

    assert(l1.contains(l1))
  }

  test("l1 doesn't contain l2 when l2 starts on earlier line") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 8, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when l2 ends on later line") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 2),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 5, 10),
      SourcePosition(source, 12, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when they start on same line and l1 ends on later line but it starts on later col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 2, 2),
      SourcePosition(source, 8, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when it starts on earlier line and they end on the same line but l1 ends on earlier col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 10),
      SourcePosition(source, 9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      SourcePosition(source, 4, 2),
      SourcePosition(source, 9, 7)
    )

    assert(!l1.contains(l2))
  }
}
