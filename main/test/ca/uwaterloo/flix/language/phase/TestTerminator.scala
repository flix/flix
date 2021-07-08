package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTerminator extends FunSuite with TestUtils {

  test("UnconditionalRecursion.01") {
    val input =
      s"""
         |def f(): Int =
         |    f()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.02") {
    val input =
      s"""
         |def foo(x: Int, y: Int): Int =
         |    foo(x, y)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.03") {
    val input =
      s"""
         |def foo(x: Int): Int = match x {
         |    case 0 => foo(999)
         |    case _ => foo(123)
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.04") {
    val input =
      s"""
         |def foo(x: Int): Int =
         |    if (x == 1)
         |        foo(9)
         |    else
         |        foo(7)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.05") {
    val input =
      s"""
         |def bar(_z: Int -> Int): Int =
         |    5
         |
         |def foo(x: Int, y: Int): Int =
         |    bar(foo(x + y))
         |
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    rejectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.06") {
    val input =
      """
        |namespace LL {
        |
        |    pub enum LazyList[t] {
        |        case Empty,
        |        case Cons(t, Lazy[LazyList[t]])
        |    }
        |
        |    pub def constant(x: a): LazyList[a] =
        |        Cons(x, lazy constant(x))
        |
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    rejectError[TerminationError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.Class.01") {
    val input =
      """
        |pub lawless class C[a] {
        |  pub def f(x: a): String = C.f(x)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
//    expectError[TerminationError.UnconditionalSigRecursion](result)
    // MATT handle sigs
  }
}
