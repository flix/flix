package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestLexer extends AnyFunSuite with TestUtils {
  test("LexerError.BlockCommentTooDeep.01") {
    val input = "/* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* this is 32 levels deep */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.BlockCommentTooDeep](result)
  }

  test("LexerError.BlockCommentTooDeep.02") {
    // Note: The innermost block-comment is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = "/* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* this is unclosed and deep */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.BlockCommentTooDeep](result)
  }

  test("LexerError.DoubleDottedNumber.01") {
    val input = "1.2.3"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.02") {
    val input = "12..3"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.03") {
    val input = "123.."
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.04") {
    val input = "123..32f32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.05") {
    val input = "12332..f32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.UnexpectedChar.01") {
    val input = "€"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnterminatedBlockComment.01") {
    val input =
      s"""
         |/* This is unterminated
         |def f(): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.02") {
    val input = "def f(): /* This is unterminated Unit = ()"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.03") {
    val input =
      s"""
         |def f(): Unit = ()
         |/* This is unterminated""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.04") {
    val input =
      s"""
         | /* /* This is unterminated and nested */
         |def f(): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBuiltIn.01") {
    val input = "$BUILT_IN; 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.02") {
    val input = "$BUILT_IN"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.03") {
    val input = "$BUILT_/*IN*/$"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.04") {
    val input = "$$BUILT_IN"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedChar.01") {
    val input = "'a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.02") {
    val input = "'\uffff"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.03") {
    val input = "'a/* This is a block-comment */'"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.04") {
    val input = "'/* This is a block-comment */a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.05") {
    val input = "'a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedInfixFunction.01") {
    val input = "1 `add 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedInfixFunction.02") {
    val input = "1 `add/*this is a block comment*/` 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedInfixFunction.03") {
    val input = "1 `add 2"
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedString.01") {
    val input = """ "This is unterminated """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.02") {
    val input = """ "\ """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedString.03") {
    val input = "\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.TerminatedStringNoNewline.01") {
    val input = """ def f(): String = "This is terminated" """
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("LexerError.UnterminatedStringInterpolation.01") {
    val input = """ "Hi ${name!" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.02") {
    val input = """ "${"Hi ${name!"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.03") {
    val input = """ "${"Hi ${name!}"" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.StringInterpolationTooDeep.01") {
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  test("LexerError.StringInterpolationTooDeep.02") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input = """ "${"${"${"${"${"${"${"${"${"${"${"${"${"${"${${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${"${unclosed and deep"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}}"}"}"}"}"}"}"}"}"}"}"}"}"}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }
}
