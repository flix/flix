/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.TypeError.UnificationError
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  // TODO: [TestTyper]: Add more negative test cases.

  /////////////////////////////////////////////////////////////////////////////
  // Unary                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary.LogicalNot.TypeError") {
    val input = "def f(): Bool = !42"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Unary.Plus.TypeError") {
    val input = "def f(): Int = +true"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Unary.Minus.TypeError") {
    val input = "def f(): Int = -true"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Unary.BitwiseNegate.TypeError") {
    val input = "def f(): Int = ~~~true"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // If Then Else                                                            //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.IfThenElse.TypeError.NonBooleanCondition") {
    val input = "def f(): Int = if (42) 1 else 2"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.IfThenElse.TypeError.MismatchedBranches") {
    val input = "def f(): Int = if (true) true else 1234"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Channels                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.NewChannel.TypeError.Unit.IllegalBuffersizeType") {
    val input = "def f(): Channel[Int] = channel Int ()"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.String.IllegalBuffersizeType") {
    val input = "def f(): Channel[Int] = channel Int \"Str\""
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Char.IllegalBuffersizeType") {
    val input = "def f(): Channel[Int] = channel Int 'a'"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Bool.IllegalBuffersizeType") {
    val input = "def f(): Channel[Int] = channel Int true"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Channel[Int].IllegalBuffersizeType") {
    val input = """def t(): Channel[Int] = channel Int
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Channel[Str].IllegalBuffersizeType") {
    val input = """def t(): Channel[Str] = channel Str
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Channel[Char].IllegalBuffersizeType") {
    val input = """def t(): Channel[Char] = channel Char
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Channel[Bool].IllegalBuffersizeType") {
    val input = """def t(): Channel[Bool] = channel Bool
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.Channel[Unit].IllegalBuffersizeType") {
    val input = """def t(): Channel[Unit] = channel Unit
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.String.IllegalReturnType") {
    val input =
      """def f(ch: Channel[Int] ): Int = select {
        |  case _ <- ch => "str"
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.Char.IllegalReturnType") {
    val input =
      """def f(ch: Channel[Int] ): Int = select {
        |  case _ <- ch => 'c'
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }
}
