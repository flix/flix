package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

/**
  * Note that CompilerSuite and LibrarySuite covers the positive testing of the parser well.
  */
class TestParserHappy extends AnyFunSuite with TestUtils {
  test("DetectRecord.01") {
    val input =
      """
        |pub def foo(): { x = Int32 } = {
        |    // This is a comment
        |    x = 1000
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("ExtensibleType.01") {
    val input =
      """
        |pub def foo(): #| A(Int32) |# = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("RecordRowExtensionAlias.01") {
    val input =
      """
        |type alias R = (y = Int32)
        |def f(a: {x = Int32 | R}): Int32 = a#x
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("RecordRowExtensionAlias.02") {
    val input =
      """
        |type alias R = (y = Int32)
        |type alias Big = (x = Int32 | R)
        |def f(a: {z = Int32 | Big }): Int32 = a#x
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }
}
