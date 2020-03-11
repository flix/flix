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

  test("Option.useReplace") {
    val input =
      s"""
         |def main(): Option[Int] =
         |    let o = Some(21);
         |    Option.map(x -> if (x == 21) 42 else x, o)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useZip") {
    val input =
      s"""
         |def main(): Option[(Int, Int)] =
         |    let o1 = Some(21);
         |    let o2 = Some(42);
         |    Option.flatMap(x -> Option.map(y -> (x, y), o2), o1)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapIdentity") {
    val input =
      s"""
         |def main(): List[Int] =
         |    List.map(x -> x, 1 :: Nil)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

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

  test("List.mapMap02") {
    val input =
      s"""
         |def main(): Int & Impure =
         |    List.length(List.map(x -> x + 1, List.map(y -> {[1, 2, 3]; y + 2}, Nil)))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapMap03") {
    val input =
      s"""
         |def main(): Int & Impure =
         |    List.length(List.map(x -> {[1, 2, 3]; x + 1}, List.map(y -> y + 2, Nil)))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  private def run(s: String): Validation[CompilationResult, CompilationError] = new Flix().setOptions(DefaultOptions).addStr(s).compile()

}
