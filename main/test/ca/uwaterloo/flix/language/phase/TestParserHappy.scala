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

  test("Use.Qualified.01") {
    // A use without a package is untouched.
    val input =
      """
        |use Game.empty
        |use Game.{size}
        |use Game.{size => boardSize}
        |mod Game {
        |    pub def empty(): Int32 = 0
        |    pub def size(): Int32 = 3
        |}
        |pub def foo(): (Int32, Int32, Int32) = (empty(), size(), boardSize())
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("Use.Qualified.02") {
    // A tight cons directly after a use is not a package separator.
    val input =
      """
        |pub def foo(): List[Int32] = {
        |    use Game.one;
        |    one()::Nil
        |}
        |mod Game {
        |    pub def one(): Int32 = 1
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibAll)
    expectSuccess(result)
  }

}
