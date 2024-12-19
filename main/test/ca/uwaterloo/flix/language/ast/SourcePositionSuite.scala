package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourcePositionSuite extends AnyFunSuite {
  test("tryCompare with different SourcePositions sources gives None") {
    val l1 = SourcePosition(
      Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray),
      0,
      0
    )
    val l2 = SourcePosition(
      Source(Input.Text("Dummy2", "dummy", SecurityContext.AllPermissions), Array.emptyCharArray),
      0,
      0
    )

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2) === None)
  }

  test("tryCompare with same SourcePosition sources gives Some") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 0, 0)
    val l2 = SourcePosition(source, 0, 0)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).isDefined)
  }

  test("tryCompare with earlier line and later col gives negative") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 2, 5)
    val l2 = SourcePosition(source, 4, 2)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n < 0))
  }

  test("tryCompare with earlier line and earlier col gives negative") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 3, 2)
    val l2 = SourcePosition(source, 4, 20)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n < 0))
  }

  test("tryCompare with later line and earlier col gives positive") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 4, 11)
    val l2 = SourcePosition(source, 1, 13)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n > 0))
  }

  test("tryCompare with later line and later col gives positive") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 4, 20)
    val l2 = SourcePosition(source, 1, 13)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n > 0))
  }

  test("tryCompare with same line and earlier col gives negative") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 4, 4)
    val l2 = SourcePosition(source, 4, 13)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n < 0))
  }

  test("tryCompare with same line and later col gives positive") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 4, 19)
    val l2 = SourcePosition(source, 4, 13)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).exists(n => n > 0))
  }

  test("tryCompare with same line and col gives 0") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val l1 = SourcePosition(source, 4, 20)
    val l2 = SourcePosition(source, 4, 20)

    assert(SourcePosition.PartialOrder.tryCompare(l1, l2).contains(0))
  }

  test("lteq with different sources gives none") {
    val p1 = SourcePosition(
      Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray),
      0,
      0
    )
    val p2 = SourcePosition(
      Source(Input.Text("Dummy2", "dummy", SecurityContext.AllPermissions), Array.emptyCharArray),
      0,
      0
    )

    assert(!SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true when p1's line is earlier line and col is later") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 2, 3)
    val p2 = SourcePosition(source, 4, 2)

    assert(SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true true when p1's line is earlier and col is earlier") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 2, 2)
    val p2 = SourcePosition(source, 4, 3)

    assert(SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false when p1's line and col is later") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 6, 3)
    val p2 = SourcePosition(source, 3, 2)

    assert(!SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false when p1's line is later and col is earlier") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 6, 2)
    val p2 = SourcePosition(source, 3, 3)

    assert(!SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true when they have same line and p1's col is earlier") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 6, 5)
    val p2 = SourcePosition(source, 6, 23)

    assert(SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false for they have same line and p1's col is later") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 6, 10)
    val p2 = SourcePosition(source, 6, 9)

    assert(!SourcePosition.PartialOrder.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true for same lines and same col") {
    val source = Source(Input.Text("Dummy1", "dummy dummy", SecurityContext.AllPermissions), Array.emptyCharArray)
    val p1 = SourcePosition(source, 6, 9)
    val p2 = SourcePosition(source, 6, 9)

    assert(SourcePosition.PartialOrder.lteq(p1, p2))
  }
}
