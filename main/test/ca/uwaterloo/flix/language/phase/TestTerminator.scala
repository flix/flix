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
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTerminator extends FunSuite with TestUtils {

  test("UnconditionalRecursion.01") {
    val input =
      s"""
         |def f(): Int32 =
         |    f()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalRecursion](result)
  }

  test("UnconditionalRecursion.02") {
    val input =
      s"""
         |def foo(x: Int32, y: Int32): Int32 =
         |    foo(x, y)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalRecursion](result)
  }

  test("UnconditionalRecursion.03") {
    val input =
      s"""
         |def foo(x: Int32): Int32 = match x {
         |    case 0 => foo(999)
         |    case _ => foo(123)
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TerminationError.UnconditionalRecursion](result)
  }

  test("UnconditionalRecursion.04") {
    val input =
      s"""
         |def foo(x: Int32): Int32 =
         |    if (x == 1)
         |        foo(9)
         |    else
         |        foo(7)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TerminationError.UnconditionalRecursion](result)
  }

  test("UnconditionalRecursion.05") {
    val input =
      s"""
         |def bar(_z: Int32 -> Int32): Int32 =
         |    5
         |
         |def foo(x: Int32, y: Int32): Int32 =
         |    bar(foo(x + y))
         |
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    rejectError[TerminationError.UnconditionalRecursion](result)
  }

  test("UnconditionalRecursion.06") {
    val input =
      """
        |namespace LL {
        |
        |    pub enum DelayList[t] {
        |        case Empty,
        |        case Cons(t, Lazy[DelayList[t]])
        |    }
        |
        |    pub def constant(x: a): DelayList[a] =
        |        Cons(x, lazy constant(x))
        |
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    rejectError[TerminationError.UnconditionalRecursion](result)
  }
}
