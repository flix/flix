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
  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.01") {
    val input = "def f(): Channel[Int] = channel Int ()"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.02") {
    val input = "def f(): Channel[Int] = channel Int 1i8"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.03") {
    val input = "def f(): Channel[Int] = channel Int 1i16"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.04") {
    val input = "def f(): Channel[Int] = channel Int 1i64"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.05") {
    val input = "def f(): Channel[Int] = channel Int 1ii"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.06") {
    val input = "def f(): Channel[Int] = channel Int \"Str\""
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.07") {
    val input = "def f(): Channel[Int] = channel Int 'a'"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.08") {
    val input = "def f(): Channel[Int] = channel Int true"
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.09") {
    val input = """def t(): Channel[Int] = channel Int
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.10") {
    val input = """def t(): Channel[Str] = channel Str
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.11") {
    val input = """def t(): Channel[Char] = channel Char
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.12") {
    val input = """def t(): Channel[Bool] = channel Bool
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.NewChannel.TypeError.IllegalChannelsizeType.13") {
    val input = """def t(): Channel[Unit] = channel Unit
                  |def f(): Channel[Int] = channel Int t()""".stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.GetChannel.TypeError.01") {
    val input = """def f(): Unit =
                  |  let ch = channel Int 3;
                  |  <- ch
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.GetChannel.TypeError.02") {
    val input = """def f(): Unit =
                  |  let ch = channel Int 3;
                  |  let x: Str = <- ch;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.GetChannel.TypeError.03") {
    val input = """def f(): Unit =
                  |  let ch = channel Unit 3;
                  |  let x: Int = <- ch;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.GetChannel.TypeError.04") {
    val input = """def f(): Unit =
                  |  let ch = channel Unit 3;
                  |  let x: Str = <- ch;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.01") {
    val input = """def f(): Unit =
                  |  let ch = channel Unit 3;
                  |  ch <- 2;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.02") {
    val input = """def f(): Unit =
                  |  let ch = channel Unit 3;
                  |  ch <- "str";
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.03") {
    val input = """def f(): Unit =
                  |  let ch = channel Unit 3;
                  |  ch <- 'a';
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.04") {
    val input = """def f(): Unit =
                  |  let ch = channel Int 3;
                  |  ch <- "str";
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.05") {
    val input = """def f(): Unit =
                  |  let ch = channel Int 3;
                  |  ch <- 'a';
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.06") {
    val input = """def f(): Unit =
                  |  let ch = channel Int 3;
                  |  ch <- ();
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.07") {
    val input = """def f(): Unit =
                  |  let ch = channel Channel[Int] 3;
                  |  ch <- 2;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.08") {
    val input = """def f(): Unit =
                  |  let ch = channel Channel[Int] 3;
                  |  ch <- "str";
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.09") {
    val input = """def f(): Unit =
                  |  let ch = channel Channel[Int] 3;
                  |  ch <- 'a';
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.10") {
    val input = """def f(): Unit =
                  |  let ch = channel Channel[Int] 3;
                  |  ch <- ();
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.11") {
    val input = """def f(): Unit =
                  |  let ch = channel List[Int] 3;
                  |  ch <- ();
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.PutChannel.TypeError.12") {
    val input = """def f(): Unit =
                  |  let ch = channel List[Int] 3;
                  |  ch <- 1;
                  |  ()
                """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Spawn.TypeError.MismatchArrowType.01") {
    val input =
      """
        |def t(): Int = 42
        |def f(): Unit = spawn t(2)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Spawn.TypeError.MismatchArrowType.02") {
    val input =
      """
        |def t(x: Int): Int = x
        |def f(): Unit = spawn t()
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.Spawn.TypeError.MismatchArrowType.03") {
    val input =
      """
        |def t(x: Int, y: Int): Int = x + y
        |def f(): Unit = spawn t(2)
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.IllegalReturnType.01") {
    val input =
      """def f(ch: Channel[Int]): Int = select {
        |  case x <- ch => "str"
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.IllegalReturnType.02") {
    val input =
      """def f(ch: Channel[Int]): Int = select {
        |  case x <- ch => 'c'
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.IllegalReturnType.03") {
    val input =
      """def f(ch1: Channel[Int], ch2: Channel[Int]): Int = select {
        |  case x <- ch1 => 2
        |  case x <- ch2 => "str"
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }

  test("Expression.SelectChannel.TypeError.IllegalReturnType.04") {
    val input =
      """def f(ch1: Channel[Int], ch2: Channel[Int]): Int = select {
        |  case x <- ch1 => 2
        |  case x <- ch2 => 'a'
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[UnificationError](result)
  }
}
