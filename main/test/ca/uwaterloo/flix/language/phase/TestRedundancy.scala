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

}
