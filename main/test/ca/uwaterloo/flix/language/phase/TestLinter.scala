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

  test("Int32.leftAdditionByZero") {
    val input =
      s"""
         |def foo(): Int = 0 + 21
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.rightAdditionByZero") {
    val input =
      s"""
         |def foo(): Int = 21 + 0
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.subtractionByZero") {
    val input =
      s"""
         |def foo(): Int = 21 - 0
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.subtractionBySelf") {
    val input =
      s"""
         |def foo(): Int =
         |    let x = 21;
         |    x - x
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.leftMultiplicationByZero") {
    val input =
      s"""
         |def foo(): Int = 0 * 21
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.rightMultiplicationByZero") {
    val input =
      s"""
         |def foo(): Int = 21 * 0
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.leftMultiplicationByOne") {
    val input =
      s"""
         |def foo(): Int = 1 * 21
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.rightMultiplicationByOne") {
    val input =
      s"""
         |def foo(): Int = 1 * 21
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.divisionByZero") {
    val input =
      s"""
         |def foo(): Int = 21 / 0
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.divisionByOne") {
    val input =
      s"""
         |def foo(): Int = 21 / 1
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Int32.divisionBySelf") {
    val input =
      s"""
         |def foo(): Int =
         |    let x = 21;
         |    x / x
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.existsSome") {
    val input =
      s"""
         |def foo(): Bool =
         |    let f = x -> x == 21;
         |    Option.exists(f, Some(42))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.flattenSome") {
    val input =
      s"""
         |def foo(): Option[Int] =
         |    Option.flatten(Some(Some(42)))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useExists") {
    val input =
      s"""
         |def foo(): Bool =
         |    let f = x -> x == 21;
         |    let o = Some(42);
         |    Option.getWithDefault(Option.map(f, o), false)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useForall") {
    val input =
      s"""
         |def foo(): Bool =
         |    let f = x -> x == 21;
         |    let o = Some(42);
         |    Option.getWithDefault(Option.map(f, o), true)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useReplace") {
    val input =
      s"""
         |def foo(): Option[Int] =
         |    let o = Some(21);
         |    Option.map(x -> if (x == 21) 42 else x, o)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useSequence") {
    val input =
      s"""
         |def foo(): Option[List[Int]] =
         |    let f = x -> if (x % 2 == 0) Some(x) else None;
         |    let xs = List.map(f, 1 :: 2 :: 3 :: Nil);
         |    Option.traverse(x -> x, xs)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useTraverse") {
    val input =
      s"""
         |def foo(): Option[List[Int]] =
         |    let f = x -> if (x % 2 == 0) Some(x) else None;
         |    Option.sequence(List.map(f, 1 :: 2 :: 3 :: Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("Option.useZip") {
    val input =
      s"""
         |def foo(): Option[(Int, Int)] =
         |    let o1 = Some(21);
         |    let o2 = Some(42);
         |    Option.flatMap(x -> Option.map(y -> (x, y), o2), o1)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.filterFilter") {
    val input =
      s"""
         |def foo(): List[Int] =
         |    List.filter(x -> x > 21, List.filter(y -> y > 42, Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.findFind") {
    val input =
      s"""
         |def foo(): Option[Int] =
         |    Option.find(x -> x > 21, List.find(y -> y > 42, Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapIdentity") {
    val input =
      s"""
         |def foo(): List[Int] =
         |    List.map(x -> x, 1 :: Nil)
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapMap01") {
    val input =
      s"""
         |def foo(): List[Int] =
         |    List.map(x -> x + 1, List.map(y -> y + 2, Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapMap02") {
    val input =
      s"""
         |def foo(): List[Int] & Impure =
         |    List.map(x -> x + 1, List.map(y -> {[1, 2, 3]; y + 2}, Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapMap03") {
    val input =
      s"""
         |def foo(): List[Int] & Impure =
         |    List.map(x -> {[1, 2, 3]; x + 1}, List.map(y -> y + 2, Nil))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("List.mapZip") {
    val input =
      s"""
         |def foo(): List[Int] =
         |    let f = (x, y) -> x + y;
         |    let xs = 1 :: 2 :: Nil;
         |    let ys = 1 :: 2 :: Nil;
         |        List.map(p -> f(fst(p), snd(p)), List.zip(xs, ys))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("String.leftConcatenateEmptyString") {
    val input =
      s"""
         |def foo(): String =
         |    "" + "hello world"
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("String.rightConcatenateEmptyString") {
    val input =
      s"""
         |def foo(): String =
         |    "hello world" + ""
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  test("String.trimTrim") {
    val input =
      s"""
         |def foo(): String =
         |    String.trim(String.trim("hello world"))
         |
       """.stripMargin
    val result = run(input)
    expectError[LinterError.Lint](result)
  }

  private def run(s: String): Validation[CompilationResult, CompilationError] = new Flix().setOptions(DefaultOptions).addStr(s).compile()

}
