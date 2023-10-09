package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestLexer extends AnyFunSuite with TestUtils {
  test("LexerError.BlockCommentTooDeep.01") {
    val input =
      s"""
         |/* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* this is 32 levels deep */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.BlockCommentTooDeep](result)
  }

  test("LexerError.BlockCommentTooDeep.02") {
    // Note: The innermost block-comment is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input =
    s"""
       |/* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* this is unclosed and deep */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.BlockCommentTooDeep](result)
  }

  test("LexerError.DoubleDottedNumber.01") {
    val input =
      s"""
         |def f(): Float32 = 1.2.3
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.02") {
    val input =
      s"""
         |def f(): Float32 = 12..3
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.03") {
    val input =
      s"""
         |def f(): Float32 = 123..
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.04") {
    val input =
      s"""
         |def f(): Float32 = 123..32f32
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.DoubleDottedNumber.05") {
    val input =
      s"""
         |def f(): Float32 = 12332..f32
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.UnexpectedChar.01") {
    val input =
      s"""
         |def f(): Char = €
       """.stripMargin
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
    val input =
      s"""
         |def f(): /* This is unterminated Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBlockComment.03") {
    val input =
      s"""
         |def f(): Unit = ()
         |/* This is unterminated
       """.stripMargin
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
    val input =
      s"""
         |def f(): Int32 = $$BUILT_IN; 42
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.02") {
    val input =
      s"""
         |def f(): Unit = $$BUILT_IN
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedBuiltIn.03") {
    val input =
      s"""
         |def f(): Unit = $$BUILT_/*IN*/$$
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBuiltIn](result)
  }

  test("LexerError.UnterminatedChar.01") {
    val input =
      s"""
         |def f(): Char = 'a
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.02") {
    val input =
      s"""
         |def f(): Char = '\uffff
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.03") {
    val input =
      s"""
         |def f(): Char = 'a/* This is a block-comment */'
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedChar.04") {
    val input =
      s"""
         |def f(): Char = '/* This is a block-comment */a'
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedInfixFunction.01") {
    val input =
      s"""
         |def f(): Int32 = 1 `add 2
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedInfixFunction.02") {
    val input =
      s"""
         |def f(): Int32 = 1 `add/*this is a block comment*/` 2
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedString.01") {
    val input =
      s"""
         |def f(): String = "This is unterminated
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedStringInterpolation.01") {
    val input =
      s"""
         |def f(): String = "Hi $${name!"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.02") {
    val input =
      s"""
         |def f(): String = "$${"Hi $${name!"}"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.UnterminatedStringInterpolation.03") {
    val input =
      s"""
         |def f(): String = "$${"Hi $${name!}""
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.StringInterpolationTooDeep.01") {
    val input =
      s"""
         |"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  test("LexerError.StringInterpolationTooDeep.02") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input =
    s"""
       |"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"this is $${unclosed and deep"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }

  test("LexerError.StringInterpolationTooDeep.02") {
    // Note: The innermost interpolation is unterminated,
    // but the lexer should stop after bottoming out so this should still be a 'too deep' error.
    val input =
    s"""
       |"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"this is $${unclosed and deep"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }
}
