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
import ca.uwaterloo.flix.language.errors.PatMatchError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestPatMatch extends AnyFunSuite with TestUtils {

  // --- A. Literal Patterns (non-enumerable types) ---

  test("Literal.Bool.01") {
    val input =
      """def f(x: Bool): Int32 = match x {
        |    case true => 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Char.01") {
    val input =
      """def f(x: Char): Int32 = match x {
        |    case 'a' => 1
        |    case 'b' => 2
        |    case 'c' => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Int8.01") {
    val input =
      """def f(x: Int8): Int32 = match x {
        |    case 1i8 => 1
        |    case 2i8 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Int16.01") {
    val input =
      """def f(x: Int16): Int32 = match x {
        |    case 1i16 => 1
        |    case 2i16 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Int32.01") {
    val input =
      """def f(x: Int32): Int32 = match x {
        |    case 1 => 1
        |    case 2 => 2
        |    case 3 => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int32 = match x {
        |    case 1i64 => 1
        |    case 2i64 => 2
        |    case 3i64 => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Float32.01") {
    val input =
      """def f(x: Float32): Int32 = match x {
        |    case 1.0f32 => 1
        |    case 2.0f32 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.Float64.01") {
    val input =
      """def f(x: Float64): Int32 = match x {
        |    case 1.0f64 => 1
        |    case 2.0f64 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.BigInt.01") {
    val input =
      """def f(x: BigInt): Int32 = match x {
        |    case 1ii => 1
        |    case 2ii => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Literal.String.01") {
    val input =
      """def f(x: String): Int32 = match x {
        |    case "foo" => 1
        |    case "bar" => 2
        |    case "baz" => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- B. Enum Patterns ---

  test("Enum.01") {
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Enum.02") {
    val input =
      """enum Color {
        |    case Red,
        |    case Grn,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |    case Color.Blu => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Enum.03") {
    val input =
      """enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Option[Color]): Int32 = match x {
        |    case Option.None    => 0
        |    case Option.Some(Color.Red) => 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Enum.04") {
    val input =
      """enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(i: Int32, xs: List[Int32]): Int32 = match (i, xs) {
        |    case (0, List.Cons(x, _)) => x
        |    case (p, List.Cons(x, rs)) => x
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Enum.05") {
    val input =
      """enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(l1: List[Int32], l2: List[Int32]): Int32 = match (l1, l2) {
        |    case (List.Nil, List.Nil) => 0
        |    case (List.Cons(x, xs), List.Cons(y, ys)) => 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Enum.06") {
    val input =
      """enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(xs: List[Int32]): Int32 = match xs {
        |    case List.Nil => 42
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- C. Tuple Patterns ---

  test("Tuple.01") {
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: (Color, Color)): Int32 = match x {
        |    case (Color.Red(), _) => 1
        |    case (_, Color.Blu) => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Tuple.02") {
    val input =
      """def f(x: (Int8, (String, String))): Int32 = match x {
        |    case (5i8, ("five", _)) => 5
        |    case (6i8, (_, "six")) => 6
        |    case (7i8, (_, _)) => 7
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Tuple.03") {
    val input =
      """def f(x: (Int32, Int32, Int32, Int32, Int32)): Int32 = match x {
        |    case (1, 2, 3, 4, 5) => 1
        |    case (_, 2, 3, 4, 5) => 1
        |    case (1, _, 3, 4, 5) => 1
        |    case (1, 2, _, 4, 5) => 1
        |    case (1, 2, 3, _, 5) => 1
        |    case (1, 2, 3, 4, _) => 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- D. Guard Patterns ---

  test("Guard.01") {
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Guard.02") {
    val input =
      """enum E {
        |    case E1,
        |    case E2,
        |    case E3
        |}
        |
        |def f(x: E): Int32 = match x {
        |    case E.E1 if true => 1
        |    case E.E2 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- E. Nested Match Expressions ---

  test("Nested.01") {
    val input =
      """
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(xs: List[Int32]): Int32 = match xs {
        |    case List.Nil => 0
        |    case List.Cons(y, ys) => match ys {
        |        case List.Nil => 0
        |        case List.Cons(z, zs) => match zs {
        |            case List.Nil => 0
        |        }
        |    }
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Nested.02") {
    val input =
      """
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |def f(l: List[Int32]): Int32 = let foo = 42;
        |    match l {
        |        case Nil => 42
        |    }
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Nested.03") {
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Nested.04") {
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- F. Expression Contexts ---

  test("Context.Instance.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |trait C[a] {
        |    pub def f(x: a): Int32
        |}
        |
        |instance C[E] {
        |    pub def f(x: E): Int32 = match x {
        |        case E.E1 => 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Context.Trait.01") {
    val input =
      """
        |enum E {
        |    case E1
        |    case E2
        |}
        |
        |trait C[a] {
        |    pub def f(_x: a): Int32 = match E.E1 {
        |        case E.E1 => 1
        |    }
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Context.MatchLambda.01") {
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Context.ParYield.01") {
    val input =
      """
        |enum E {
        |    case E1(Int32)
        |    case E2
        |}
        |
        |def f(): Int32 = par (E.E1(x) <- if (true) E.E1(1) else E.E2) yield x
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // --- G. Redundant Pattern Tests ---

  test("Redundant.Wildcard.01") {
    val input =
      """def f(x: Int32): Int32 = match x {
        |    case _ => 1
        |    case _ => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Wildcard.02") {
    val input =
      """def f(x: Int32): Int32 = match x {
        |    case _ => 1
        |    case 42 => 2
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Bool.01") {
    val input =
      """def f(x: Bool): Int32 = match x {
        |    case true => 1
        |    case false => 2
        |    case _ => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Enum.01") {
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |    case Color.Blu => 2
        |    case _ => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Enum.02") {
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |    case Color.Blu => 2
        |    case Color.Red => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Nested.01") {
    val input =
      """enum Option[t] {
        |    case None,
        |    case Some(t)
        |}
        |
        |def f(x: Option[Bool]): Int32 = match x {
        |    case Option.None => 0
        |    case Option.Some(true) => 1
        |    case Option.Some(false) => 2
        |    case Option.Some(_) => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Guard.01") {
    // A guarded rule should NOT make later rules redundant.
    // Here, the guard on the wildcard means `Color.Red` is still reachable.
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case _ if true => 1
        |    case Color.Red => 2
        |    case Color.Blu => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Guard.02") {
    // A guarded rule IS reported as redundant if its pattern is unreachable.
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |    case Color.Blu => 2
        |    case _ if true => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.Tuple.01") {
    val input =
      """def f(x: (Bool, Bool)): Int32 = match x {
        |    case (true, true) => 1
        |    case (true, false) => 2
        |    case (false, true) => 3
        |    case (false, false) => 4
        |    case (_, _) => 5
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.NoFalsePositive.01") {
    // No redundancy: each enum case is distinct.
    val input =
      """enum Color {
        |    case Red,
        |    case Grn,
        |    case Blu
        |}
        |
        |def f(x: Color): Int32 = match x {
        |    case Color.Red => 1
        |    case Color.Grn => 2
        |    case Color.Blu => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.RedundantPattern](result)
  }

  test("Redundant.NoFalsePositive.02") {
    // No redundancy: wildcard is needed for Int32 (infinite type).
    val input =
      """def f(x: Int32): Int32 = match x {
        |    case 1 => 1
        |    case 2 => 2
        |    case _ => 3
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.RedundantPattern](result)
  }

  // --- H. Record Pattern Tests ---

  // H.1 Non-Exhaustive

  test("Record.NonExhaustive.01") {
    // Single Bool field, one case missing.
    val input =
      """def f(): Bool = match { x = true } {
        |    case { x = true } => true
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.02") {
    // Single Int32 field (infinite type), one constant.
    val input =
      """
        |def f(): Bool = match { x = 1 } {
        |    case { x = 1 } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.03") {
    // Single enum field, one case missing.
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.04") {
    // Multi-field Bool, one field incomplete.
    val input =
      """def f(): Bool = match { x = true, y = false } {
        |    case { x = true, y = _ } => true
        |    case { x = false, y = true } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.05") {
    // Mixed Unit (complete) + Int32 (infinite) fields.
    val input =
      """
        |def f(): Bool = match { x = 1, y = () } {
        |    case { x = 1, y = () } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.06") {
    // Extension pattern with non-exhaustive Int32 field.
    val input =
      """
        |def f(): Bool = match { x = 1, y = () } {
        |    case { x = 1 | _ } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.07") {
    // Extension pattern with enum non-exhaustiveness.
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { x = A.A, y = A.B } {
        |    case { x = A.A | _ } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.08") {
    // Deeply nested records.
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
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.NonExhaustive.09") {
    // Duplicate labels — exercises CanonicalKey (name, idx) system.
    val input =
      """
        |enum A {
        |    case A,
        |    case B
        |}
        |
        |def f(): Bool = match { a = A.A, a = A.B } {
        |    case { a = A.A, a = A.A } => true
        |    case { a = A.B, a = A.A } => true
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // H.2 Exhaustive (no false positive)

  test("Record.Exhaustive.01") {
    // Single Bool field, both cases covered.
    val input =
      """def f(): Bool = match { x = true } {
        |    case { x = true } => true
        |    case { x = false } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.Exhaustive.02") {
    // Single Bool field, wildcard covers all.
    val input =
      """def f(): Bool = match { x = true } {
        |    case { x = _ } => true
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.Exhaustive.03") {
    // Enum field, all cases covered.
    val input =
      """enum Color {
        |    case Red,
        |    case Blu
        |}
        |
        |def f(x: {c = Color}): Bool = match x {
        |    case { c = Color.Red } => true
        |    case { c = Color.Blu } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.Exhaustive.04") {
    // Different field orders should still be exhaustive together.
    val input =
      """def f(): Bool = match { a = true, b = false } {
        |    case { a = true, b = _ } => true
        |    case { b = _, a = false } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.NonExhaustiveMatch](result)
  }

  test("Record.Exhaustive.05") {
    // Extension patterns should be exhaustive.
    val input =
      """def f(r: {a = Bool | _}): Bool = match r {
        |    case { a = true | _ } => true
        |    case { a = false | _ } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    rejectError[PatMatchError.NonExhaustiveMatch](result)
  }

  // H.3 Redundant

  test("Record.Redundant.01") {
    // Wildcard followed by wildcard.
    val input =
      """def f(): Bool = match { x = true } {
        |    case { x = _ } => true
        |    case { x = _ } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

  test("Record.Redundant.02") {
    // Bool complete, then wildcard is redundant.
    val input =
      """def f(): Bool = match { x = true } {
        |    case { x = true } => true
        |    case { x = false } => false
        |    case { x = _ } => false
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[PatMatchError.RedundantPattern](result)
  }

}
