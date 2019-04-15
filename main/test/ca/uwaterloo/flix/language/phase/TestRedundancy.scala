package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestRedundancy extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("UnusedEnumSym.01") {
    val input =
      s"""
         |enum Color {
         |  case Red,
         |  case Green,
         |  case Blue
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.02") {
    val input =
      s"""
         |enum One {
         |  case A(Two)
         |}
         |
         |enum Two {
         |  case B(One)
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.03") {
    val input =
      s"""
         |type USD = USD(Int)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumTag.01") {
    val input =
      s"""
         |enum Color {
         |  case Red,
         |  case Green,
         |  case Blue
         |}
         |
         |def main(): Color = Red
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedFormalParam.01") {
    val input =
      s"""
         |def f(x: Int): Int = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.02") {
    val input =
      s"""
         |def f(x: Int, y: Int): Int = y
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.03") {
    val input =
      s"""
         |def f(x: Int, y: Int): Int = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.04") {
    val input =
      s"""
         |def f(x: Int, y: Int, z: Int): Int = x + z
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.05") {
    val input =
      s"""
         |def f(): Int =
         |  let f = x -> 123;
         |  f(1)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.06") {
    val input =
      s"""
         |def f(): Int =
         |  let f = (x, y) -> x;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.07") {
    val input =
      s"""
         |def f(): Int =
         |  let f = (x, y) -> y;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.09") {
    val input =
      s"""
         |def f(): Bool = \\forall(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.10") {
    val input =
      s"""
         |def f(): Bool = \\exists(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

}
