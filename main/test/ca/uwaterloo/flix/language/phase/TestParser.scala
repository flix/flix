package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{LexerError, ParseError, WeederError}
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
    expectError[ParseError](result)
  }

  // Produces ResolutionError with no SourceLocation, which fails the test.
  ignore("ParseError.Interpolation.02") {
    val input = s"""pub def foo(): String = "$${1 + }""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
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

  // TODO: Move, this should be a Parse error
  test("IllegalEffectTypeParams.01") {
    val input =
      """
        |eff MyEffect[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  // TODO: Move, this should be a Parse error
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

  // TODO: Move, this should be a Parse error
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

  // TODO: Move, This is a parse error, did not expect effect on an operation
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

  // TODO: Move, This is a parse error, did not expect effect on an operation
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

  // TODO: Move, This is a parse error, did not expect effect on an operation
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

  // TODO: Move, This is a parse error, did not expect effect on an operation
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

  // TODO: This is a parser error
  test("IllegalEnum.02") {
    val input =
      """
        |enum E(Int32) { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  // TODO: This is a parser error
  test("IllegalEnum.03") {
    val input =
      """
        |enum E(a) { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  // TODO: Move. Parse error "Expected UppercaseName found LowerCaseName"
  test("IllegalModuleName.01") {
    val input =
      """
        |mod mymod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  // TODO: Move. Parse error "Expected UppercaseName found LowerCaseName"
  test("IllegalModuleName.02") {
    val input =
      """
        |mod Mymod.othermod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  // TODO: Move. Parse error "Expected UppercaseName found LowerCaseName"
  test("IllegalModuleName.03") {
    val input =
      """
        |mod Mymod {
        |    mod othermod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  // TODO: Move: Parse error "Expected NameLowerCase found NameUpperCase".
  // Testing that the casing of 'foo' and 'Foo' should match
  test("IllegalUse.Alias.01") {
    val input =
      """
        |mod M {
        |    def foo(): Int32 = ???
        |}
        |
        |mod N {
        |    use M.{foo => Foo}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  // TODO: Move: Parse error "Expected NameLowerCase found NameUpperCase".
  test("IllegalUse.Alias.02") {
    val input =
      """
        |mod M {
        |    enum Enum1
        |    def foo(): Int32 = ???
        |}
        |
        |mod N {
        |    use M.{Enum1 => enum1}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  // TODO: Move: Parse error "Expected NameLowerCase found NameUpperCase".
  test("IllegalUse.Alias.03") {
    val input =
      """
        |mod N {
        |    use M.{E => e}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  // TODO: Move: Parse error "Expected NameLowerCase found NameUpperCase".
  test("IllegalUse.Alias.04") {
    val input =
      """
        |mod B {
        |    use M.{e => A}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }
}
