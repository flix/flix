package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{LexerError, ParseError, WeederError}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import org.scalatest.Suites

class TestParser extends Suites(
  // TODO: Enable these tests once the new parser is the only one in use.
  // new TestParserRecovery,
  new TestParserHappy
)

/**
  * Recover tests are cases where we would like to check that the parser recovers from a syntax error.
  * Each test contains one or more syntax error.
  * The parser should understand all the surrounding code but still produce a parse error for the syntax mistake.
  * That is asserted by checking that main is defined in the compilation result.
  * There is an theoretically infinite amount of possible syntax errors (we could just start fuzzing random strings),
  * so these test cover some "sensible amount" of broad errors.
  * Some areas that could use more test are:
  * - Declarations other than definitions (module, enum, trait, signature, effect).
  * - Patterns
  * - Map, Set, List, Vector and Array literals.
  * - "Niche" expressions (OpenAs, JVMops, Fixpoint expressions).
  *
  * Note that these tests use [[check]] rather than [[compile]].
  * That's because a compile converts any check failure into a HardFailure before running, codegen so the result we would like to expect is lost.
  */
class TestParserRecovery extends AnyFunSuite with TestUtils {

  test("Use.01") {
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

  test("Use.02") {
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

  test("Use.03") {
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

  test("Import.01") {
    val input =
      """
        |import java.lang.{StringBuffer,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("Import.02") {
    val input =
      """
        |import java.lang.{StringBuffer => StrBuf
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("Import.03") {
    val input =
      """
        |import java.lang.{StringBuffer, , CharSequence};
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("Parameters.01") {
    val input =
      """
        |def foo(x: Int32, , z: Int32): Int32 = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("Parameters.02") {
    val input =
      """
        |def foo(x: Int32,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("NoDefBody.01") {
    val input =
      """
        |def foo(x: Int32): Int32
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("NoDefType.01") {
    val input =
      """
        |def foo(x: Int32) = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadTrait.01") {
    val input =
      """
        |trait DoesNothing[a
        |  pub def noop(x: a): Unit = ???
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.01") {
    val input =
      """
        |enum Legumes { Chickpea, Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.02") {
    val input =
      """
        |enum Legumes[a { case Chickpea(a), case Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.03") {
    val input =
      """
        |enum USD[a](a
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.01") {
    val input =
      """
        |type alias M[k, = Map[k, Result[String, k]]
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.02") {
    val input =
      """
        |type alias M[k] = Map[k, Result[String, ]
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadCallExpr.01") {
    val input =
      """
        |def foo(x: Int32, y: Int32): Int32 = bar(1,
        |// Note: We need an enum here. If we just had main, it would get consumed as a LetRecDef.
        |enum Legumes { case ChickPea, Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadCall.02") {
    val input =
      """
        |def foo(x: Int32, y: Int32): Int32 = {
        |  bar( 1 + ;
        |  let x =
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadQualifiedPath.01") {
    val input =
      """
        |def foo(): In32 = Bar.
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadRecordSelect.01") {
    val input =
      """
        |def foo(): In32 = bar().
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadBinaryOperation.01") {
    val input =
      """
        |def foo(): Int32 = { 1 + }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadBinaryOperation.02") {
    val input =
      """
        |def foo(): Int32 = % 2
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadWithout.01") {
    val input =
      """
        |def foo(): Int32 = 1 without { IO
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadWithout.02") {
    val input =
      """
        |def foo(): Int32 = 1 without
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadLambda.01") {
    val input =
      """
        |def foo(): Int32 = { () -> ; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadLambda.02") {
    val input =
      """
        |def foo(): Int32 = { (a, ) -> ; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadTuple.01") {
    val input =
      """
        |def foo(): (Int32, Int32) = (1, )
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadUnary.01") {
    val input =
      """
        |def foo(): Int32 = { lazy; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.01") {
    val input =
      """
        |def foo(): Int32 = if 123 else 321
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.02") {
    val input =
      """
        |def foo(): Int32 = if () 123 else 321
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.03") {
    val input =
      """
        |def foo(): Int32 = if (false) 123
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.01") {
    val input =
      """
        |def foo(): Int32 = {
        | let (_x, ) = (1, 2);
        | 123
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.02") {
    val input =
      """
        |def foo(): Int32 = {
        | let (, x  = (1, 2);
        | x
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.03") {
    val input =
      """
        |def foo(): Int32 = {
        | let x:  = enum;
        | x
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadMatch.01") {
    val input =
      """
        |def foo(): Int32 = match () { case }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadType.01") {
    val input =
      """
        |def foo(): List[Int32 = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadType.02") {
    val input =
      """
        |def foo(): Int32 -> = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadType.03") {
    val input =
      """
        |def foo(): Int32 -> Int32 \ { = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }

  test("BadType.04") {
    val input =
      """
        |def foo(): #{ Node() | = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectErrorOnCheck[ParseError](result)
    expectMain(result)
  }
}

/**
  * Note that CompilerSuite and LibrarySuite covers the positive testing of the parser well.
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
