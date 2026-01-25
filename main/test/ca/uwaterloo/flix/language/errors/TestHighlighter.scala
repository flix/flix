package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.{Formatter, Options}
import ca.uwaterloo.flix.util.Formatter.{AnsiTerminalFormatter, NoFormatter}
import org.scalatest.funsuite.AnyFunSuite

class TestHighlighter extends AnyFunSuite with TestUtils {

  //
  // Category 1: TypeError Single-Line Tests
  //

  test("Highlighter.TypeError.MismatchedTypes.01") {
    val input = """def foo(): Int32 = "hello""""
    val errors = getErrors(input)
    assert(errors.nonEmpty)
    val output = formatErrors(errors)
    assertSingleLineFormat(output)
  }

  test("Highlighter.TypeError.MismatchedTypes.02") {
    val input =
      """
        |def foo(): String = 123
        |""".stripMargin
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assertSingleLineFormat(output)
    assert(output.contains("123") || output.contains("String"))
  }

  test("Highlighter.TypeError.OverApplied.01") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123)
        |""".stripMargin
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assertSingleLineFormat(output)
  }

  //
  // Category 2: LexerError Tests
  //

  test("Highlighter.LexerError.ExpectedDigit.01") {
    val input = "12..3"
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assert(output.contains("|"))
  }

  test("Highlighter.LexerError.UnterminatedString.01") {
    val input =
      """
        |def foo(): String = "hello
        |""".stripMargin
    val errors = getErrors(input)
    val output = formatErrors(errors)
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
    val errors = getErrors(input)
    val output = formatErrors(errors)
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
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assert(output.contains("|"))
  }

  test("Highlighter.RedundancyError.HiddenVarSym.01") {
    val input =
      """
        |def f(): Int32 =
        |    let _x = 123;
        |    _x
        |""".stripMargin
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assertSingleLineFormat(output)
  }

  //
  // Category 5: Edge Cases
  //

  test("Highlighter.EdgeCase.ShortSpan.01") {
    val input = """def foo(): Int32 = x"""
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assert(output.contains("^"))
  }

  test("Highlighter.EdgeCase.LongSpan.01") {
    val input =
      """def veryLongFunctionName(): Int32 = "this is definitely not an integer value""""
    val errors = getErrors(input)
    val output = formatErrors(errors)
    assertSingleLineFormat(output)
  }

  //
  // Category 6: Formatter Tests
  //

  test("Highlighter.Formatter.NoFormatter.01") {
    val input = """def foo(): Int32 = "string""""
    val errors = getErrors(input)
    val output = formatErrors(errors, NoFormatter)
    assert(!output.contains("\u001b"), "NoFormatter should not produce ANSI codes")
  }

  test("Highlighter.Formatter.AnsiTerminalFormatter.01") {
    val input = """def foo(): Int32 = "string""""
    val errors = getErrors(input)
    val output = formatErrors(errors, AnsiTerminalFormatter)
    assert(output.contains("\u001b"), "AnsiTerminalFormatter should produce ANSI codes")
  }

  //
  // Category 7: Syntax Highlighting with Root
  //

  test("Highlighter.SyntaxHighlight.WithRoot.01") {
    val input =
      """
        |def used(): Int32 = 42
        |def foo(): Int32 = used() + "string"
        |""".stripMargin
    val (optRoot, errors) = getErrorsWithRoot(input)
    val output = formatErrors(errors, AnsiTerminalFormatter)(optRoot)
    assert(output.contains("\u001b"))
  }

  test("Highlighter.SyntaxHighlight.WithoutRoot.01") {
    val input = """def foo(): Int32 = "string""""
    val errors = getErrors(input)
    val output = formatErrors(errors, NoFormatter)(None)
    assert(!output.contains("\u001b"))
  }

  //
  // Helper Functions
  //

  private def formatErrors(errors: List[CompilationMessage], fmt: Formatter = NoFormatter)
                          (implicit root: Option[TypedAst.Root] = None): String = {
    CompilationMessage.formatAll(errors)(fmt, root)
  }

  private def getErrors(input: String, options: Options = Options.TestWithLibNix): List[CompilationMessage] = {
    val (_, errors) = check(input, options)
    errors
  }

  private def getErrorsWithRoot(input: String, options: Options = Options.TestWithLibNix): (Option[TypedAst.Root], List[CompilationMessage]) = {
    check(input, options)
  }

  private def assertSingleLineFormat(output: String): Unit = {
    assert(output.contains(" | "), "Expected ' | ' separator")
    assert(output.contains("^"), "Expected caret underline")
  }
}
