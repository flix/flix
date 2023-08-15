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
import ca.uwaterloo.flix.language.errors.NonExhaustiveMatchError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestPatExhaustiveness extends AnyFunSuite with TestUtils {

  test("Pattern.Literal.Char.01") {
    val input =
      """def f(x: Char): Int32 = match x {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Int32.01") {
    val input =
      """def f(x: Int32): Int32 = match x {
        |  case 1 => 1
        |  case 2 => 2
        |  case 3 => 3
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int32 = match x {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: String): Int32 = match x {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Tuples.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: (Color, Color)): Int32 = match x {
        |  case (Color.Red() ,_ ) => 1
        |  case (_, Color.Blu) => 2
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Tuples.02") {
    val input =
      """def f(x: (Int8, (String, String))): Int32 = match x {
        |  case (5i8, ("five", _)) => 5
        |  case (6i8, (_, "six")) => 6
        |  case (7i8, (_,_)) => 7
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Tuples.03") {
    val input =
      """def f(x: (Int32, Int32, Int32, Int32, Int32)): Int32 = match x {
        |  case (1,2,3,4,5) => 1
        |  case (_,2,3,4,5) => 1
        |  case (1,_,3,4,5) => 1
        |  case (1,2,_,4,5) => 1
        |  case (1,2,3,_,5) => 1
        |  case (1,2,3,4,_) => 1
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Lists.01") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(i: Int32, xs: IntList): Int32 = match (i, xs) {
        |  case (0, IntList.Lst(x, _)) => x
        |  case (p, IntList.Lst(x, rs)) => x
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Lists.02") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(l1: IntList, l2: IntList): Int32 = match (l1, l2) {
        |  case (IntList.Empty, IntList.Empty) => 0
        |  case (IntList.Lst(x,xs), IntList.Lst(y,ys)) => 1
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Literal.Lists.03") {
    val input =
      """ enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(xs: IntList): Int32 = match xs {
        |  case IntList.Empty => 42
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Expression.LetMatch01") {
    val input =
      """enum E {
        |  case A(Bool, Char, Int8)
        |}
        |
        |def f(e: E): Int8 = let E.A(true, 'a', i) = e; i
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Expression.LetMatch02") {
    val input =
      """def f(e: (Int8, Int8)): Int8 = let (a,1i8) = e; a
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Deep.01") {
    val input =
      """enum Evil {
        |  case Evil(Evil, Evil),
        |  case Good
        |}
        |
        |def f(x: Evil): Evil = match x {
        |  case Evil.Evil(_, Evil.Evil(_, Evil.Evil(_, Evil.Evil(_, Evil.Evil(_, Evil.Evil(_, Evil.Evil(_, _))))))) => Evil.Evil(Evil.Good, Evil.Good)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Expression.MatchLambda.01") {
    val input =
      """
        |enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |def f(): Option[Int32] -> Int32 = match None -> 42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Expression.MatchLambda.02") {
    val input =
      """
        |enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |def f(): Option[Int32] -> Int32 = match Some(x) -> x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Nested.01") {
    val input =
      """
        |enum IntList {
        |   case Lst(Int32, IntList),
        |   case Empty
        |}
        |def f(xs: IntList): Int32 = match xs {
        |  case IntList.Empty => 0
        |  case IntList.Lst(y,ys) => match ys {
        |      case IntList.Empty => 0
        |      case IntList.Lst(z,zs) => match zs {
        |           case IntList.Empty => 0
        |      }
        |  }
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Nested.02") {
    val input =
      """
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(l: List[Int32]): Int32 = let foo = 42 ;
        |     match l {
        |         case Nil => 42
        |     }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Nested.03") {
    val input =
      """
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(l: List[Int32]): Int32 = {
        |    match (match l { case Nil => Nil }) {
        |        case _ => 42
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Nested.04") {
    val input =
      """
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(l: List[Int32]): Int32 = {
        |    match l {
        |        case _ if (match Nil { case Nil => true }) => 53
        |        case _ => 42
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Instance.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |class C[a] {
        |    pub def f(x: a): Int32
        |}
        |
        |instance C[E] {
        |    pub def f(x: E): Int32 = match x {
        |        case E.E1 => 1
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Class.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |class C[a] {
        |    pub def f(_x: a): Int32 = match E.E1 {
        |        case E.E1 => 1
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.ParYield.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |def f(): E = par (E.E1 <- if (true) E.E1 else E.E2) yield E.E1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Guard.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |def f(): Int32 = match E.E1 {
        |    case E.E1 if true => 123
        |    case E.E2 => 456
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.01") {
    val input =
      """
        |def f(): Bool = match { x = 1 } {
        |    case { x = 1 } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.02") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = () } {
        |    case { x = 1 | r } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.03") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = 2 } {
        |    case { x = 1, y = 2 } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.04") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = 2 } {
        |    case { x = 1, y = 2 | _ } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.05") {
    val input =
      """
        |def f(): Bool = match { x = 1, y = () } {
        |    case { x = 1, y = () } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.06") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = A.A } {
        |    case { x = A.A } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.07") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = A.A, y = A.B } {
        |    case { x = A.A | r } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.08") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = A.A, y = A.B } {
        |    case { x = A.A, y = A.B } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.09") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = { x = { }, y = A.A }, y = A.B } {
        |    case { x = { x = { }, y = A.A }, y = A.B } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.10") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = { x = { }, y = A.A }, y = A.B } {
        |    case { x = { x = { }, y = A.A | r }, y = A.B } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.11") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = { x = { }, y = A.A }, y = A.B } {
        |    case { x = { y = A.A | r }, y = A.B } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }

  test("Pattern.Record.12") {
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = { x = { }, y = A.A }, y = A.B } {
        |    case { x = { x = { } | r }, y = A.B } => true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NonExhaustiveMatchError](result)
  }
}
