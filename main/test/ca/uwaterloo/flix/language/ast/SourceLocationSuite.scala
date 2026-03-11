package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.CompilerConstants
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourceLocationSuite extends AnyFunSuite {

  private val Src = Source.empty(Input.VirtualFile(CompilerConstants.VirtualTestFile, "dummy dummy", SecurityContext.Unrestricted))

  test("l1 contains l2 when l1 starts on earlier line and ends on later") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 9),
      SourcePosition.mkFromOneIndexed(10, 3)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(4, 0),
      SourcePosition.mkFromOneIndexed(9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start on same line and earlier col and l1 ends on later line and earlier col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(4, 3),
      SourcePosition.mkFromOneIndexed(10, 3)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(4, 4),
      SourcePosition.mkFromOneIndexed(9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when l1 starts on earlier line and later col and ends on same line and later col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 11)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(4, 4),
      SourcePosition.mkFromOneIndexed(9, 10)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and l1 starts on earlier col and ends on later col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(2, 12),
      SourcePosition.mkFromOneIndexed(9, 2)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and l1 starts on earlier col and ends on same col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(2, 12),
      SourcePosition.mkFromOneIndexed(9, 4)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they start and end on same line and l1 starts on same col and ends on later col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 2)
    )

    assert(l1.contains(l2))
  }

  test("l1 contains itself") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )

    assert(l1.contains(l1))
  }

  test("l1 doesn't contain l2 when l2 starts on earlier line") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(5, 2),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(8, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when l2 ends on later line") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 2),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(5, 10),
      SourcePosition.mkFromOneIndexed(12, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when they start on same line and l1 ends on later line but it starts on later col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(2, 2),
      SourcePosition.mkFromOneIndexed(8, 2)
    )

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when it starts on earlier line and they end on the same line but l1 ends on earlier col") {
    val l1 = SourceLocation(
      isReal = true,
      Src,
      SourcePosition.mkFromOneIndexed(2, 10),
      SourcePosition.mkFromOneIndexed(9, 4)
    )
    val l2 = SourceLocation(
      isReal = false,
      Src,
      SourcePosition.mkFromOneIndexed(4, 2),
      SourcePosition.mkFromOneIndexed(9, 7)
    )

    assert(!l1.contains(l2))
  }
}
