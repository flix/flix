package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.CompilerConstants
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import org.scalatest.funsuite.AnyFunSuite

class SourceLocationSuite extends AnyFunSuite {

  private val Src = Source.empty(Input.VirtualFile(CompilerConstants.VirtualTestFile, "dummy dummy", SecurityContext.Unrestricted))

  test("l1 contains l2 when l2 is fully enclosed in l1") {
    val l1 = SourceLocation(isReal = true, Src, 2, 10)
    val l2 = SourceLocation(isReal = false, Src, 3, 9)

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they share end point") {
    val l1 = SourceLocation(isReal = true, Src, 2, 9)
    val l2 = SourceLocation(isReal = false, Src, 3, 9)

    assert(l1.contains(l2))
  }

  test("l1 contains l2 when they share start point") {
    val l1 = SourceLocation(isReal = true, Src, 2, 9)
    val l2 = SourceLocation(isReal = false, Src, 2, 8)

    assert(l1.contains(l2))
  }

  test("l1 contains itself") {
    val l1 = SourceLocation(isReal = true, Src, 2, 9)

    assert(l1.contains(l1))
  }

  test("l1 doesn't contain l2 when l2 starts earlier") {
    val l1 = SourceLocation(isReal = true, Src, 5, 9)
    val l2 = SourceLocation(isReal = false, Src, 2, 8)

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when l2 ends later") {
    val l1 = SourceLocation(isReal = true, Src, 2, 9)
    val l2 = SourceLocation(isReal = false, Src, 5, 12)

    assert(!l1.contains(l2))
  }

  test("l1 doesn't contain l2 when l2 starts earlier and shared end point") {
    val l1 = SourceLocation(isReal = true, Src, 3, 9)
    val l2 = SourceLocation(isReal = false, Src, 2, 9)

    assert(!l1.contains(l2))
  }
}
