package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestLinter extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  ignore("TrivialExpression.LeftAdditionByZero") {
    val input =
      """
        |pub def f(): Int = 0 + 123
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[LinterError.TrivialExpression](result)
  }

  // TODO: Add tests

}
