package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{LexerError, Parser2Error}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

/**
 * This test suite is very sparse.
 * Note that CompilerSuite and LibrarySuite covers the positive testing of the parser well.
 * We would like more negative tests in here though.
 */
class TestParser extends AnyFunSuite with TestUtils {
  // Produces ResolutionError with no SourceLocation, which fails the test.
  ignore("ParseError.Interpolation.01") {
    val input = s"""pub def foo(): String = "$${""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  // Produces ResolutionError with no SourceLocation, which fails the test.
  ignore("ParseError.Interpolation.02") {
    val input = s"""pub def foo(): String = "$${1 + }""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  // Produces ResolutionError with no SourceLocation, which fails the test.
  ignore("ParseError.Interpolation.03") {
    val input = s"""pub def foo(): String = "$${1 {}""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("ParseError.EnumCase.01") {
    val input =
      """
        |enum E {
        |    case C()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  test("ParseError.ParYield.01") {
    val input =
      """
        |def f(): Int32 = par () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  test("ParseError.ParYield.02") {
    val input =
      """
        |def f(): Int32 = par a <- 1 yield a
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  test("ParseError.ParYield.03") {
    val input =
      """
        |def f(): (Int32, Int32) = par (a <- let b = 1; b; c <- 2) yield (a, c)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  test("ParseError.InstanceOf.01") {
    val input =
      """
        |def foo(): Bool =
        |    1000ii instanceof java.math.BigInteger
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }

  test("ParseError.InstanceOf.02") {
    val input =
      """
        |def foo(): Bool =
        |    1000ii instanceof BigInt
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[Parser2Error](result)
  }
}
