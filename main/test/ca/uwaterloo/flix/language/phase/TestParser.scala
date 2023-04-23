package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.ParseError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestParser extends AnyFunSuite with TestUtils {

  test("ParseError.Int.01") {
    val input =
      s"""
         |def f(): Int = 1_
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int.02") {
    val input =
      s"""
         |def f(): Int = 1_000_
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int.03") {
    val input =
      s"""
         |def f(): Int = 0x_1
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int.04") {
    val input =
      s"""
         |def f(): Int = 0x1_
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int8.01") {
    val input =
      s"""
         |def f(): Int8 = 1_i8
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int8.02") {
    val input =
      s"""
         |def f(): Int8 = 1_000_i8
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int8.03") {
    val input =
      s"""
         |def f(): Int8 = 0x_1i8
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int8.04") {
    val input =
      s"""
         |def f(): Int8 = 0x1_i8
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int16.01") {
    val input =
      s"""
         |def f(): Int16 = 1_i16
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int16.02") {
    val input =
      s"""
         |def f(): Int16 = 1_000_i16
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int16.03") {
    val input =
      s"""
         |def f(): Int16 = 0x_1i16
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int16.04") {
    val input =
      s"""
         |def f(): Int16 = 0x1_i16
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int32.01") {
    val input =
      s"""
         |def f(): Int32 = 1_i32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int32.02") {
    val input =
      s"""
         |def f(): Int32 = 1_000_i32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int32.03") {
    val input =
      s"""
         |def f(): Int32 = 0x_1i32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int32.04") {
    val input =
      s"""
         |def f(): Int32 = 0x1_i32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int64.01") {
    val input =
      s"""
         |def f(): Int64 = 1_i64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int64.02") {
    val input =
      s"""
         |def f(): Int64 = 1_000_i64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int64.03") {
    val input =
      s"""
         |def f(): Int64 = 0x_1i64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Int64.04") {
    val input =
      s"""
         |def f(): Int64 = 0x1_i64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigInt.01") {
    val input =
      s"""
         |def f(): BigInt = 1_ii
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigInt.02") {
    val input =
      s"""
         |def f(): BigInt = 1_000_ii
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigInt.03") {
    val input =
      s"""
         |def f(): BigInt = 0x_1ii
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigInt.04") {
    val input =
      s"""
         |def f(): BigInt = 0x1_ii
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float.01") {
    val input =
      s"""
         |def f(): Float = 1_.0
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float.02") {
    val input =
      s"""
         |def f(): Float = 1_000_.0
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float.03") {
    val input =
      s"""
         |def f(): Float = 1.0_
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float32.01") {
    val input =
      s"""
         |def f(): Float32 = 1_.0f32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float32.02") {
    val input =
      s"""
         |def f(): Float32 = 1_000_.0f32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float32.03") {
    val input =
      s"""
         |def f(): Float32 = 1.0_f32
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float64.01") {
    val input =
      s"""
         |def f(): Float64 = 1_.0f64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float64.02") {
    val input =
      s"""
         |def f(): Float64 = 1_000_.0f64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Float64.03") {
    val input =
      s"""
         |def f(): Float64 = 1.0_f64
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigDecimal.01") {
    val input =
      s"""
         |def f(): BigDecimal = 1_.0ff
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigDecimal.02") {
    val input =
      s"""
         |def f(): BigDecimal = 1_000_.0ff
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.BigDecimal.03") {
    val input =
      s"""
         |def f(): Float64 = 1.0_ff
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Regression.01") {
    val input =
      s"""
         |def foo(): String = "abc"
         |def bar(): String = foo()
         |d
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.EOI.01") {
    val input = """def foo(): String = """"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.EOI.02") {
    val input =
      """def foo(): Char = '"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.EOI.03") {
    val input =
      """def foo (): String = "\"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.EOI.04") {
    val input = """def foo (): Char = "\"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.01") {
    val input = s"""pub def foo(): String = "$${""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.02") {
    val input = s"""pub def foo(): String = "$${1 + }""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.Interpolation.03") {
    val input = s"""pub def foo(): String = "$${1 + {2}""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.EnumCase.01") {
    val input =
      """
        |enum E {
        |    case C()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.01") {
    val input =
      """
        |def f(): Int32 = par () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.02") {
    val input =
      """
        |def f(): Int32 = par a <- 1 yield a
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("ParseError.ParYield.03") {
    val input =
      """
        |def f(): (Int32, Int32) = par (a <- let b = 1; b; c <- 2) yield (a, c)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }
}
