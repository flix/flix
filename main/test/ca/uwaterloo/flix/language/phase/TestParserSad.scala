package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{LexerError, ParseError, WeederError}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

/**
  * Tests errors without testing recovery.
  */
class TestParserSad extends AnyFunSuite with TestUtils {
  test("ParseError.Interpolation.01") {
    val input = s"""pub def foo(): String = "$${1 + }""""
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.02") {
    val input = s"""pub def foo(): String = "$${1 {}""""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("ParseError.Interpolation.03") {
    val input = """pub def foo(): String = "\\${""""
    val result = check(input, Options.TestWithLibNix)
    expectError[LexerError](result)
  }

  test("ParseError.ParYield.01") {
    val input =
      """
        |def f(): Int32 = par a <- 1 yield a
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.02") {
    val input =
      """
        |def f(): (Int32, Int32) = par (a <- let b = 1; b; c <- 2) yield (a, c)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.InstanceOf.01") {
    val input =
      """
        |def foo(): Bool =
        |    1000ii instanceof java.math.BigInteger
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalEffectTypeParams.01") {
    val input =
      """
        |eff MyEffect[a]
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectTypeParams.02") {
    val input =
      """
        |eff MyEffect {
        |    def op[a](x: a): Unit
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectTypeParams.03") {
    val input =
      """
        |eff MyEffect[a] {
        |    def op[b](x: a, y: b): Unit
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalExtMatchRule.01") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) { }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalExtTag.01") {
    val input =
      """
        |def f(): Int32 = ematch xvar A() -> 123 {
        |    case A(x) => x
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalOperationWithOutReturnType.01") {
    val input =
      """
        |eff E{
        |    def op()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalOperationWithOutReturnType.02") {
    val input =
      """
        |eff E{
        |    def op():
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalEffectfulOperation.01") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ IO
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEffectfulOperation.02") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ E
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEffectfulOperation.03") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ ef
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
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
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalEnum.01") {
    val input =
      """
        |enum E(Int32) { }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  test("IllegalEnum.02") {
    val input =
      """
        |enum E(a) { }
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  test("IllegalModule.01") {
    val input =
      """
        |mod DelayMap {
        |    @Experimental
        |    pub de
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalModuleName.01") {
    val input =
      """
        |mod mymod {
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalModuleName.02") {
    val input =
      """
        |mod Mymod.othermod {
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("IllegalModuleName.03") {
    val input =
      """
        |mod Mymod {
        |    mod othermod {
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("BadThrow.01") {
    val input =
      """
        |def foo(): Unit \ IO = throw
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("NewStructNoBody.01") {
    val input =
      """
        |def foo(s: S[r]): Int32 = new Struct @ r |> ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("Nested.Mod.Eff") {
    val input =
      """
        |eff E {
        |    mod
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("Nested.Mod.Instance") {
    val input =
      """
        |instance E {
        |    mod
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("Nested.Mod.Trait") {
    val input =
      """
        |trait E {
        |    mod
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("BadTupleEnd.01") {
    val input =
      """
        |def foo(x: Int32): (Int32, Int32) = (x, |)
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("MinusCase.NoBrace.01") {
    val input =
      """
        |def foo(): Int32 = match 0 {
        |  case -
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("MinusCase.YesBrace.01") {
    val input =
      """
        |def foo(): Int32 = match 0 {
        |  case -
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("MinusCase.Capital.01") {
    val input =
      """
        |def foo(): Int32 = match 0 {
        |  case -ABC
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("NeedAtleastOne.01") {
    val input =
      """
        |trait A[] {
        |    def f(x: a): a
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("ExpectedArrowThickRGotEqual.02") {
    val input =
      """
        |def f(): Int32 = match 42 {
        |      case x = x + 1
        |      case _ => 0
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotEqual](result)
  }

  test("ExpectedArrowThickRGotEqual.03") {
    val input =
      """
        |def map(): Option[Int32] = {
        |    let m = Map#{"a" = 1, "b" => 2, "c" => 3};
        |    Map.get("b", m)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotEqual](result)
  }

  test("ExpectedArrowThickRGotEqual.04") {
    val input =
      """
        |import java.io.BufferedReader
        |import java.io.File
        |import java.io.FileReader
        |import java.io.FileNotFoundException
        |import java.io.IOException
        |
        |def main(): Unit \ IO =
        |    let f = new File("foo.txt");
        |    try {
        |        let r = new BufferedReader(new FileReader(f));
        |        let l = r.readLine();
        |        println("The first line of the file is: ${l}");
        |        r.close()
        |    } catch {
        |        case _: FileNotFoundException =>
        |            println("The file does not exist!")
        |        case ex: IOException =
        |            println("The file could not be read!");
        |            println("The error message was: ${ex.getMessage()}")
        |    }
        |
        |""".stripMargin

    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotEqual](result)
  }

  test("ExpectedArrowThickRGotArrowThinR.01") {
    val input =
      """
        |def f(): Int32 = match 42 {
        |      case x -> x + 1
        |      case _ => 0
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotArrowThinR](result)
  }

  test("ExpectedArrowThickRGotArrowThinR.02") {
    val input =
      """
        |def map(): Option[Int32] = {
        |    let m = Map#{"a" -> 1, "b" => 2, "c" => 3};
        |    Map.get("b", m)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotArrowThinR](result)
  }

  test("ExpectedArrowThickRGotArrowThinR.03") {
    val input =
      """
        |def map(): Option[Int32] = {
        |    let m = Map#{"a"->1, "b" => 2, "c" => 3};
        |    Map.get("b", m)
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotArrowThinR](result)
  }

  test("ExpectedArrowThickRGotArrowThinR.04") {
    val input =
      """
        |import java.io.BufferedReader
        |import java.io.File
        |import java.io.FileReader
        |import java.io.FileNotFoundException
        |import java.io.IOException
        |
        |def main(): Unit \ IO =
        |    let f = new File("foo.txt");
        |    try {
        |        let r = new BufferedReader(new FileReader(f));
        |        let l = r.readLine();
        |        println("The first line of the file is: ${l}");
        |        r.close()
        |    } catch {
        |        case _: FileNotFoundException =>
        |            println("The file does not exist!")
        |        case ex: IOException ->
        |            println("The file could not be read!");
        |            println("The error message was: ${ex.getMessage()}")
        |    }
        |
        |""".stripMargin

    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError.ExpectedArrowThickRGotArrowThinR](result)
  }
}
