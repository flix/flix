package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourcePositionSuite extends AnyFunSuite {

  test("compare with earlier line and later col gives negative") {
    val l1 = SourcePosition.mkFromOneIndexed(2, 5)
    val l2 = SourcePosition.mkFromOneIndexed(4, 2)

    assert(SourcePosition.Order.compare(l1, l2) < 0)
  }

  test("compare with earlier line and earlier col gives negative") {
    val l1 = SourcePosition.mkFromOneIndexed(3, 2)
    val l2 = SourcePosition.mkFromOneIndexed(4, 20)

    assert(SourcePosition.Order.compare(l1, l2) < 0)
  }

  test("compare with later line and earlier col gives positive") {
    val l1 = SourcePosition.mkFromOneIndexed(4, 11)
    val l2 = SourcePosition.mkFromOneIndexed(1, 13)

    assert(SourcePosition.Order.compare(l1, l2) > 0)
  }

  test("compare with later line and later col gives positive") {
    val l1 = SourcePosition.mkFromOneIndexed(4, 20)
    val l2 = SourcePosition.mkFromOneIndexed(1, 13)

    assert(SourcePosition.Order.compare(l1, l2) > 0)
  }

  test("compare with same line and earlier col gives negative") {
    val l1 = SourcePosition.mkFromOneIndexed(4, 4)
    val l2 = SourcePosition.mkFromOneIndexed(4, 13)

    assert(SourcePosition.Order.compare(l1, l2) < 0)
  }

  test("compare with same line and later col gives positive") {
    val l1 = SourcePosition.mkFromOneIndexed(4, 19)
    val l2 = SourcePosition.mkFromOneIndexed(4, 13)

    assert(SourcePosition.Order.compare(l1, l2) > 0)
  }

  test("compare with same line and col gives 0") {
    val l1 = SourcePosition.mkFromOneIndexed(4, 20)
    val l2 = SourcePosition.mkFromOneIndexed(4, 20)

    assert(SourcePosition.Order.compare(l1, l2) == 0)
  }

  test("p1 lteq p2 gives true when p1's line is earlier line and col is later") {
    val p1 = SourcePosition.mkFromOneIndexed(2, 3)
    val p2 = SourcePosition.mkFromOneIndexed(4, 2)

    assert(SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true true when p1's line is earlier and col is earlier") {
    val p1 = SourcePosition.mkFromOneIndexed(2, 2)
    val p2 = SourcePosition.mkFromOneIndexed(4, 3)

    assert(SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false when p1's line and col is later") {
    val p1 = SourcePosition.mkFromOneIndexed(6, 3)
    val p2 = SourcePosition.mkFromOneIndexed(3, 2)

    assert(!SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false when p1's line is later and col is earlier") {
    val p1 = SourcePosition.mkFromOneIndexed(6, 2)
    val p2 = SourcePosition.mkFromOneIndexed(3, 3)

    assert(!SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true when they have same line and p1's col is earlier") {
    val p1 = SourcePosition.mkFromOneIndexed(6, 5)
    val p2 = SourcePosition.mkFromOneIndexed(6, 23)

    assert(SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives false for they have same line and p1's col is later") {
    val p1 = SourcePosition.mkFromOneIndexed(6, 10)
    val p2 = SourcePosition.mkFromOneIndexed(6, 9)

    assert(!SourcePosition.Order.lteq(p1, p2))
  }

  test("p1 lteq p2 gives true for same lines and same col") {
    val p1 = SourcePosition.mkFromOneIndexed(6, 9)
    val p2 = SourcePosition.mkFromOneIndexed(6, 9)

    assert(SourcePosition.Order.lteq(p1, p2))
  }
}
