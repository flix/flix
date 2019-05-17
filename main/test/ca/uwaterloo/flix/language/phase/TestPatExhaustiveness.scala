/*
 * Copyright 2015-2016 Jason Mittertreiner
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
import ca.uwaterloo.flix.language.errors.NonExhaustiveMatchError
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestPatExhaustiveness extends FunSuite with TestUtils {

  /**
    * Runs Flix on the given input string `s`.
    */
  def run(s: String, core: Boolean = true): CompilationResult = {
    new Flix().setOptions(Options.DefaultTest.copy(core = core)).addStr(s).compile().get
  }

  test("Pattern.Literal.Char.01") {
    val input =
      """def f(x: Char): Int = match x with {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Int32.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case 1 => 1
        |  case 2 => 2
        |  case 3 => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int = match x with {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Tuples.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int = match x with {
        |  case (Color.Red() ,_ ) => 1
        |  case (_, Color.Blu) => 2
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Tuples.02") {
    val input =
      """def f(x: (Int8, (Str, Str))): Int = match x with {
        |  case (5i8, ("five", _)) => 5
        |  case (6i8, (_, "six")) => 6
        |  case (7i8, (_,_)) => 7
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Tuples.03") {
    val input =
      """def f(x: (Int, Int, Int, Int, Int)): Int = match x with {
        |  case (1,2,3,4,5) => 1
        |  case (_,2,3,4,5) => 1
        |  case (1,_,3,4,5) => 1
        |  case (1,2,_,4,5) => 1
        |  case (1,2,3,_,5) => 1
        |  case (1,2,3,4,_) => 1
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Lists.01") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(i: Int32, xs: IntList): Int32 = match (i, xs) with {
        |  case (0, Lst(x, _)) => x
        |  case (p, Lst(x, rs)) => x
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Lists.02") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(l1: IntList, l2: IntList): Int32 = match (l1, l2) with {
        |  case (Empty, Empty) => 0
        |  case (Lst(x,xs), Lst(y,ys)) => 1
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Literal.Lists.03") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(xs: IntList): Int32 = match xs with {
        |  case Empty => 42
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.LetMatch01") {
    val input =
      """enum E {
        |  case A(Bool, Char, Int8)
        |}
        |
        |def f(e: E): Int8 = let E.A(true, 'a', i) = e; i
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.LetMatch02") {
    val input =
      """def f(e: (Int8, Int8)): Int8 = let (a,1i8) = e; a
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Deep.01") {
    val input =
      """enum Evil {
        |  case Evil(Evil, Evil),
        |  case Done
        |}
        |
        |def f(x: Evil): Evil = match x with {
        |  case Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, Evil(_, _))))))) => Evil(Done, Done)
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.MatchLambda.01") {
    val input = "def f(): Option[Int] -> Int = match None -> 42"
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Expression.MatchLambda.02") {
    val input = "def f(): Option[Int] -> Int = match Some(x) -> x"
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Nested.01") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(xs: IntList): Int32 = match xs with {
        |  case Empty => 0
        |  case Lst(y,ys) => match ys with {
        |      case Empty => 0
        |      case Lst(z,zs) => match zs with {
        |           case Empty => 0
        |      }
        |  }
        |}
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

  test("Pattern.Nested.02") {
    val input =
      """ def f(l: List[Int]): Int = let foo = 42 ;
        |     match l with {
        |         case Nil => 42
        |     }
      """.stripMargin
    expectError[NonExhaustiveMatchError](new Flix().addStr(input).compile())
  }

}
