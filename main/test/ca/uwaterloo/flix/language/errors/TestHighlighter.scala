package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.{Formatter, Options}
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import org.scalatest.funsuite.AnyFunSuite

class TestHighlighter extends AnyFunSuite with TestUtils {

  //
  // Category 1: TypeError Single-Line Tests
  //

  test("Highlighter.TypeError.MismatchedTypes.01") {
    val input = """def foo(): Int32 = "hello""""
    val (root, errors) = getErrorsWithRoot(input)
    assert(errors.nonEmpty)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertSingleLineFormat(output)
  }

  test("Highlighter.TypeError.MismatchedTypes.02") {
    val input =
      """
        |def foo(): String = 123
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertSingleLineFormat(output)
    assert(output.contains("123") || output.contains("String"))
  }

  test("Highlighter.TypeError.OverApplied.01") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123)
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertSingleLineFormat(output)
  }

  //
  // Category 2: LexerError Tests
  //

  test("Highlighter.LexerError.ExpectedDigit.01") {
    val input = "12..3"
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.contains("|"))
  }

  test("Highlighter.LexerError.UnterminatedString.01") {
    val input =
      """
        |def foo(): String = "hello
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.nonEmpty)
  }

  //
  // Category 3: NameError Tests
  //

  test("Highlighter.NameError.DuplicateLowerName.01") {
    val input =
      """
        |def foo(): Int32 = 1
        |def foo(): Int32 = 2
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.contains("Duplicate") || output.contains("duplicate"))
    assert(output.contains("|"))
  }

  //
  // Category 4: RedundancyError Tests
  //

  test("Highlighter.RedundancyError.ShadowedName.01") {
    val input =
      """
        |def f(x: Int32): Int32 =
        |    let x = 123;
        |    x
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.contains("|"))
  }

  test("Highlighter.RedundancyError.HiddenVarSym.01") {
    val input =
      """
        |def f(): Int32 =
        |    let _x = 123;
        |    _x
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertSingleLineFormat(output)
  }

  //
  // Category 5: Edge Cases
  //

  test("Highlighter.EdgeCase.ShortSpan.01") {
    val input = """def foo(): Int32 = x"""
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.contains("^"))
  }

  test("Highlighter.EdgeCase.LongSpan.01") {
    val input =
      """def veryLongFunctionName(): Int32 = "this is definitely not an integer value""""
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertSingleLineFormat(output)
  }

  //
  // Category 6: Formatter Tests
  //

  test("Highlighter.Formatter.AnsiTerminalFormatter.01") {
    val input = """def foo(): Int32 = "string""""
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assert(output.contains("\u001b"), "AnsiTerminalFormatter should produce ANSI codes")
  }

  //
  // Category 7: Multi-Line Tests
  //

  test("Highlighter.MultiLine.MatchExpression.01") {
    val input =
      """
        |def foo(x: Int32): String = match x {
        |    case 1 => "one"
        |    case 2 => "two"
        |    case _ => 999
        |}
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertMultiLineFormat(output)
  }

  test("Highlighter.MultiLine.MatchExpression.02") {
    val input =
      """
        |def bar(x: Bool): Int32 = match x {
        |    case true => 1
        |    case false => "not a number"
        |}
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertMultiLineFormat(output)
  }

  test("Highlighter.MultiLine.IfThenElse.01") {
    val input =
      """
        |def baz(x: Bool): Int32 =
        |    if (x)
        |        42
        |    else
        |        "not an int"
        |""".stripMargin
    val (root, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter, root)
    assertMultiLineFormat(output)
  }

  //
  // Helper Functions
  //

  private def formatErrors(errors: List[CompilationMessage], fmt: Formatter, root: Option[TypedAst.Root]): String = {
    CompilationMessage.formatAll(errors)(fmt, root)
  }

  private def getErrorsWithRoot(input: String, options: Options = Options.TestWithLibNix): (Option[TypedAst.Root], List[CompilationMessage]) = {
    check(input, options)
  }

  private def assertSingleLineFormat(output: String): Unit = {
    assert(output.contains(" | "), "Expected ' | ' separator")
    assert(output.contains("^"), "Expected caret underline")
  }

  private def assertMultiLineFormat(output: String): Unit = {
    assert(output.contains(" | "), "Expected ' | ' separator")
    // Multi-line format shows multiple numbered lines (e.g., "2 | ", "3 | ", "4 | ")
    val lineNumberPattern = "\\d+ \\| ".r
    val matches = lineNumberPattern.findAllIn(output).toList
    assert(matches.size >= 2, s"Expected at least 2 line numbers in multi-line format, found ${matches.size}")
  }
}
