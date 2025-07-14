package ca.uwaterloo.flix.library

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestAssert extends AnyFunSuite with TestUtils {

  test("Assert.eq.01") {
    val input =
      """
        |def f(): Bool = {
        |    Assert.eq(false, true)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectSuccess(result)
  }

}
