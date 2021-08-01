/*
 * Copyright 2021 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestKinder extends FunSuite with TestUtils {

  private val DefaultOptions = Options.TestWithLibNix

  ////// TESTS COPIED FROM OTHER PHASES //////

  test("MismatchedTypeParamKind.Implicit.01") {
    val input = "def f(g: Int -> o & o): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Implicit.02") {
    val input = "def f(g: Int -> Int & e): e = g(123)"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Implicit.03") {
    val input = "def f(s: #{| a}, r: {| a}): Int = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Implicit.04") {
    val input = "def f(s: #{X(Int) | a}, r: {x: Int | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Implicit.05") {
    val input = "def f(a: e): Int & not e = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Implicit.06") {
    val input =
      """
        |enum E[a] {
        |  case E1(a)
        |}
        |
        |def f(g: E[a -> b & e]): Int & not (a or b) = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }


  test("MismatchedTypeParamKind.Enum.01") {
    val input =
      """
        |enum E[o] {
        |    case A(Int -> o & o)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Enum.02") {
    val input =
      """
        |enum E[e] {
        |    case A((Int -> Int & e) -> e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Enum.03") {
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, {| a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Enum.04") {
    val input =
      """
        |enum E[a] {
        |    case A(#{X(Int) | a}, {x: Int | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Enum.05") {
    val input =
      """
        |enum E[e] {
        |    case A(e -> Int & not e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.Enum.06") {
    val input =
      """
        |enum D[a] {
        |  case D1(a)
        |}
        |enum E[a, b, e] {
        |    case A(D[a -> b & e] -> Int & not (a or b))
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.01") {
    val input = "type alias T[o] = Int -> o & o"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.02") {
    val input = "type alias T[e] = (Int -> Int & e) -> e"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.03") {
    val input = "type alias T[a] = (#{| a}, {| a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.04") {
    val input = "type alias T[a] = (#{X(Int) | a}, {x: Int | a})"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.05") {
    val input = "type alias T[e] = e -> Int & not e"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("MismatchedTypeParamKind.TypeAlias.06") {
    val input =
      """
        |enum Option[a] {
        |  case Some(a)
        |  case None
        |}
        |
        |type alias T[a, b, e] = Option[a -> b & e] -> Int & not (a or b)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.02") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P[Int])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }


  test("IllegalUninhabitedType.03") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.04") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.05") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |def f(p: P[Int, Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.06") {
    val input =
      """
        |enum P[a, b, c] {
        |  case C(a, b, c)
        |}
        |
        |enum E {
        |  case A(P[Int, Int])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.07") {
    val input = """def f(x: true): Int = 123"""
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.08") {
    val input = "def f(): Int = 1 as Pure"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.09") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int = 1 as E[Int]""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.10") {
    val input = "def f(): Int = 1: Pure"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalUninhabitedType.11") {
    val input =
      """
        |enum E[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(): Int = 1: E[Int]""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalEffect.01") {
    val input = "def f(): Int = 1 as & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalEffect.02") {
    val input = "def f(): Int = 1 as Int & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalEffect.03") {
    val input = "def f(): Int = 1: & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
  }

  test("IllegalEffect.04") {
    val input = "def f(): Int = 1: Int & Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int, String, String]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.02") {
    val input =
      """
        |type alias R = {x: Int}
        |
        |def f(p: R[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.03") {
    val input =
      """
        |rel A(a: Int)
        |
        |type alias S = #{ A }
        |
        |def f(p: S[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.04") {
    val input = "def f(p: String[Int]): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.05") {
    val input = "def f(): Int = 1 as Int & Int and true"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.06") {
    val input = "def f(): Int = 1 as Int & true or Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.07") {
    val input = "def f(): Int = 1 as Int & not Int"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("IllegalTypeApplication.08") {
    val input = "def f(a: (Int, true)): Int = 1"
    val result = compile(input, DefaultOptions)
    expectError[KindError](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  ////// NEW TESTS START HERE //////

  test("KindError.Def.Effect.01") {
    val input =
      """
        |def f(): Unit & Unit = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Effect.02") {
    val input =
      """
        |def f[a: Type](): Unit & a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Expression.Ascribe.01") {
    val input =
      """
        |def f(): Int = 1: Pure
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Expression.Ascribe.02") {
    val input =
      """
        |def f(): Int = 1: & Unit
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Expression.Cast.01") {
    val input =
      """
        |def f(): Int = 1 as Pure
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Expression.Cast.02") {
    val input =
      """
        |def f(): Int = 1 as & Unit
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Type.01") {
    val input =
      """
        |def f(x: Int[Int]): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Type.02") {
    val input =
      """
        |def f(x: Int -> Int & Int): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Type.03") {
    val input =
      """
        |def f(x: Pure -> Int & Int): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Type.04") {
    val input =
      """
        |def f[r: Type](x: {name: Int | r} ): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Type.05") {
    val input =
      """
        |def f[r: Type](x: #{| r} ): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Parameter.01") {
    val input =
      """
        |def f(x: Pure): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Parameter.02") {
    val input =
      """
        |enum E[a]
        |
        |def f(x: E): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Return.01") {
    val input =
      """
        |def f(): Pure = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Return.02") {
    val input =
      """
        |enum E[a]
        |
        |def f(): E = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.TypeConstraint.01") {
    val input =
      """
        |class C[a: Type -> Type]

        |def f[a: Type](): a with C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Mismatch.01") {
    val input =
      """
        |def f(x: a): Int & a = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Mismatch.02") {
    val input =
      """
        |class C[a: Type -> Type]
        |
        |def f(x: a): Int with C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Def.Mismatch.03") {
    val input =
      """
        |def f(x: a -> a & a): Int = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Case.01") {
    val input =
      """
        |enum E {
        |  case C(Pure)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Case.02") {
    val input =
      """
        |enum F[a]
        |
        |enum E {
        |  case C(F)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Case.03") {
    val input =
      """
        |enum E[a: Type -> Type] {
        |  case C(a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Case.04") {
    val input =
      """
        |enum E[a] {
        |  case C({i: Int | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Type.01") {
    val input =
      """
        |enum E {
        |  case C(Int -> Int & Int)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Enum.Type.02") {
    val input =
      """
        |enum E[a] {
        |  case C(Int -> Int & a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Instance.Def.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): a
        |}
        |
        |enum E[a]
        |
        |instance C[E[a]] {
        |  pub def f(x: E): E = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Instance.TypeConstraint.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a: Type -> Type]
        |
        |enum E[a]
        |
        |instance C[E[a]] with D[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Instance.TypeParameter.01") {
    val input =
      """
        |class C[a]
        |
        |enum E[a]
        |
        |instance C[E]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.TypeAlias.Type.01") {
    val input =
      """
        |type alias T = Pure -> Int
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.TypeAlias.Type.02") {
    val input =
      """
        |type alias T[a] = Int -> Int & a
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.TypeAlias.Type.03") {
    val input =
      """
        |rel A(x: Int)
        |
        |type alias Z[r] = #{ A | r }
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Class.Law.01") {
    val input =
      """
        |class C[a: Type -> Type] {
        |  law l: forall (x: a) . ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Class.Sig.01") {
    val input =
      """
        |class C[a: Type -> Type] {
        |  pub def f(x: a): Int = ???
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.MismatchedKinds](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }

  test("KindError.Class.TypeConstraint.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a: Type -> Type] with C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[KindError.UnexpectedKind](result)
    result match {
      case ca.uwaterloo.flix.util.Validation.Success(_) => ()
      case ca.uwaterloo.flix.util.Validation.Failure(errs) => errs.foreach(err => println(err.message.fmt(ca.uwaterloo.flix.util.vt.TerminalContext.AnsiTerminal)))
    }
  }
}
