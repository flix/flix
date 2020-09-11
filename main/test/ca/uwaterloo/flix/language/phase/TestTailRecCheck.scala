package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.NonTailRecursiveCallError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTailRecCheck extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("NonTailRecursiveCallError.01") {
    val input =
      s"""
         |@tailrec
         |def fact(i: Int): Int =
         |  if (i == 1)
         |    1
         |  else
         |    i * fact(i - 1)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NonTailRecursiveCallError](result)
  }

  test("NonTailRecursiveCallError.02") {
    val input =
      s"""
         |@tailrec
         |def fact(i: Int, acc: Int): Int =
         |  if (i == 1)
         |    acc
         |  else
         |    fact(i - 1, acc * i)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    result.get
  }
}
