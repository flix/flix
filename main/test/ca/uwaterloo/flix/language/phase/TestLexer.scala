package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.LexerError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestLexer extends AnyFunSuite with TestUtils {
  test("LexerError.BlockCommentTooDeep") {
    val input =
      s"""
         |/* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* /* this is 32 levels deep */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */ */
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.BlockCommentTooDeep](result)
  }

  test("LexerError.DoubleDottedNumber") {
    val input =
      s"""
         |def f(): Float32 = 1.2.3
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.DoubleDottedNumber](result)
  }

  test("LexerError.UnexpectedChar") {
    val input =
      s"""
         |def f(): Char = €
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnexpectedChar](result)
  }

  test("LexerError.UnterminatedBlockComment") {
    val input =
      s"""
         |/* This is unterminated
         |def f(): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedBlockComment](result)
  }

  test("LexerError.UnterminatedBuiltIn") {
    val input =
      s"""
         |def f(): Unit = $$BUILT_IN
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
         |def f(): Char = '0xffff
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedChar](result)
  }

  test("LexerError.UnterminatedInfixFunction") {
    val input =
      s"""
         |def f(): Int32 = 1 `add 2
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedInfixFunction](result)
  }

  test("LexerError.UnterminatedString") {
    val input =
      s"""
         |def f(): String = "This is unterminated
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedString](result)
  }

  test("LexerError.UnterminatedStringInterpolation") {
    val input =
      s"""
         |def f(): String = "Hi $${name!"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.UnterminatedStringInterpolation](result)
  }

  test("LexerError.StringInterpolationTooDeep") {
    val input =
      s"""
         |"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${"$${}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"}"
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[LexerError.StringInterpolationTooDeep](result)
  }
}
