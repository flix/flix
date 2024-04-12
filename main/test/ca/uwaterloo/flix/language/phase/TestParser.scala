package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{LexerError, ParseError, WeederError}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import org.scalatest.Suites

class TestParser extends Suites(
  new TestParserRecovery,
  new TestParserHappy
)

/**
  * Recover tests are cases where we would like to check that the parser recovers from a syntax error.
  * Each test contains one or more syntax error.
  * The parser should understand all the surrounding code but still produce a parse error for the syntax mistake.
  * That is asserted by checking that main is defined in the compilation result.
  * Note that these tests use [[check]] rather than [[compile]].
  * That's because a compile converts any check failure into a HardFailure before running, codegen so the result we would like to expect is lost.
  * TODO: These tests need TestWithLibMin. We would like to use TestWithLibNix because that's minimal but it produces "key not found: printUnlessUnit".
  */
class TestParserRecovery extends AnyFunSuite with TestUtils {

  test("RecoverUse.01") {
    val input =
      """
        |use Color.{Red;
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverUse.02") {
    val input =
      """
        |use Color.{Red =>
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverUse.03") {
    val input =
      """
        |use Color.{Red => ScarletRed;
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverImport.01") {
    val input =
      """
        |import java.lang.{StringBuffer,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverImport.02") {
    val input =
      """
        |import java.lang.{StringBuffer => StrBuf
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverImport.03") {
    val input =
      """
        |import java.lang.{StringBuffer, , CharSequence};
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverParameters.01") {
    val input =
      """
        |def foo(x: Int32, , z: Int32): Int32 = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverParameters.02") {
    val input =
      """
        |def foo(x: Int32,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverNoDefBody.01") {
    val input =
      """
        |def foo(x: Int32): Int32
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("RecoverNoDefType.01") {
    val input =
      """
        |def foo(x: Int32) = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }
}

/**
  * Note that CompilerSuite and LibrarySuite covers the positive testing of the parser well.
  * We would like more negative tests in here though.
  */
class TestParserHappy extends AnyFunSuite with TestUtils {
  test("ParseError.Interpolation.01") {
    val input = s"""pub def foo(): String = "$${""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.02") {
    val input = s"""pub def foo(): String = "$${1 + }""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.03") {
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
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.01") {
    val input =
      """
        |def f(): Int32 = par () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.02") {
    val input =
      """
        |def f(): Int32 = par a <- 1 yield a
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.03") {
    val input =
      """
        |def f(): (Int32, Int32) = par (a <- let b = 1; b; c <- 2) yield (a, c)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.InstanceOf.01") {
    val input =
      """
        |def foo(): Bool =
        |    1000ii instanceof java.math.BigInteger
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.InstanceOf.02") {
    val input =
      """
        |def foo(): Bool =
        |    1000ii instanceof BigInt
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalEffectTypeParams.01") {
    val input =
      """
        |eff MyEffect[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectTypeParams.02") {
    val input =
      """
        |eff MyEffect {
        |    def op[a](x: a): Unit
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectTypeParams.03") {
    val input =
      """
        |eff MyEffect[a] {
        |    def op[b](x: a, y: b): Unit
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectfulOperation.01") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ IO
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEffectfulOperation.02") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ E
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEffectfulOperation.03") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ ef
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEffectfulOperation.04") {
    val input =
      """
        |eff A {
        |    pub def op(): Unit
        |}
        |eff E {
        |    def op(): Unit \ A
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEnum.01") {
    val input =
      """
        |enum E(Int32) { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  test("IllegalEnum.02") {
    val input =
      """
        |enum E(a) { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  // TODO: unignore with Parser2
  ignore("IllegalModuleName.01") {
    val input =
      """
        |mod mymod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  // TODO: unignore with Parser2
  ignore("IllegalModuleName.02") {
    val input =
      """
        |mod Mymod.othermod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  // TODO: unignore with Parser2
  ignore("IllegalModuleName.03") {
    val input =
      """
        |mod Mymod {
        |    mod othermod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }
}
