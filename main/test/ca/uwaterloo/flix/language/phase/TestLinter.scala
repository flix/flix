package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

class TestLinter extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.Default.copy(xlinter = true)

  test("List.mapMap01") {
    val input =
      s"""
         |def main(): Int =
         |    List.length(List.map(x -> x + 1, List.map(y -> y + 2, Nil)))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }



  private def run(s: String): Validation[CompilationResult, CompilationError] = new Flix().setOptions(DefaultOptions).addStr(s).compile()

}
