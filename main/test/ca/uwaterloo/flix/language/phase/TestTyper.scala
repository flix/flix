/*
 * Copyright 2020 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTyper extends AnyFunSuite with TestUtils {

  test("TestLeq01") {
    val input =
      """
        |def foo(): a = 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq02") {
    val input =
      """
        |def foo(): List[a] = 21 :: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq03") {
    val input =
      """
        |def foo(): Result[a, Int32] = Ok(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq04") {
    val input =
      """
        |def foo(): Result[Int32, a] = Err(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq05") {
    val input =
      """
        |def foo(): a -> a = x -> 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq06") {
    val input =
      """
        |def foo(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq07") {
    val input =
      """
        |def foo(): {x = Int32 | r} = {x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq08") {
    val input =
      """
        |def foo(): {x = Int32, y = Int32 | r} = {y = 42, x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestOccurs01") {
    val input = "def foo(a: #{A(Int32) | r}, b: #{B(Int32) | r}): #{A(Int32), B(Int32)} = solve (a <+> b)"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.OccursCheckError](result)
  }

  test("TestMismatchedTypes.01") {
    val input = "def foo(): {| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TestMismatchedTypes.02") {
    val input = "def foo(): #{| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TestMismatchedTypes.03") {
    val input = "def foo(): {a = Int32} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TestMismatchedTypes.04") {
    val input = "def foo(): String = solve \"hello\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("TestOverApplied.01") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.OverApplied](result)
  }

  test("TestOverApplied.02") {
    val input =
      """
        |def f(s: String): String = s
        |def over(): String = f("hello", 123, true)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.OverApplied](result)
  }

  test("TestUnderApplied.01") {
    val input =
      """
        |def f(x: String, y: Int32): Bool = true
        |def under(): String = f("hello"): String
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnderApplied](result)
  }

  test("TestUnderApplied.02") {
    val input =
      """
        |def f(x: String, y: Int32, z: Bool): Bool = true
        |def under(): String = f("hello"): String
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnderApplied](result)
  }

  test("TestLeq.Wildcard.01") {
    val input = "def foo(a: _): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.02") {
    val input = "def foo(a: Int32): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.03") {
    val input = raw"def foo(a: Int32): Int32 \ _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectGeneralizationError](result)
  }

  test("TestLeq.Wildcard.04") {
    val input = raw"def foo(g: Int32 -> Int32 \ _): Int32 \ _ = g(1)"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("NoMatchingInstance.01") {
    val input =
      """
        |class C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: a): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.02") {
    val input =
      """
        |class C[a] {
        |  pub def foo(x: a): String
        |}
        |def foo(x: Int32): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.03") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Float64]): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.04") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a] {
        |    pub def foo(x: a): String
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): String = "123"
        |}
        |
        |instance C[Box[a]] with C[a] {
        |    pub def foo(x: Box[a]): String = match x {
        |        case Box.Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Int32]): String = C.foo(C.foo(x))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.05") {
    val input =
      """
        |class C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |instance C[Int32] {
        |    pub def foo(x: Int32): Int32 = x
        |}
        |
        |def bar(x: a, y: Int32): (Int32, Int32) = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.06") {
    val input =
      """
        |class C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |def bar(x: a, y: b): (Int32, Int32) with C[a] = (C.foo(x), C.foo(y))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.07") {
    val input =
      """
        |class C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |enum E[_: Eff] {
        |    case E(Int32)
        |}
        |
        |instance C[E[Pure]] {
        |    pub def foo(x: E[Pure]): Int32 = 1
        |}
        |
        |def bar(): Int32 = C.foo(E.E(123))    // E(123) has type E[_], not E[Pure]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
  }

  test("NoMatchingInstance.Relation.01") {
    val input =
      """
        |pub enum E {
        |   case E1
        |}
        |
        |pub def f(): Bool = {
        |   let _x = #{
        |     R(E.E1).
        |   };
        |   true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingEq](result)
  }

  test("MissingEq.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x == y
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingEq](result)
  }

  test("MissingSendable.01") {
    val input =
      """
        |enum NotSendable(Int32)
        |enum TrySendable[a](a) with Sendable
        |
        |def requiresSendable(x: a): a with Sendable[a] = x
        |
        |def foo(): TrySendable[NotSendable] = requiresSendable(TrySendable.TrySendable(NotSendable.NotSendable(42)))
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingSendable](result)
  }

  test("MissingSendable.02") {
    val input =
      """
        |enum NotSendable(Int32)
        |enum TrySendable[a, b](a, b) with Sendable
        |
        |def requiresSendable(x: a): a with Sendable[a] = x
        |
        |def foo(): TrySendable[NotSendable, NotSendable] = requiresSendable(TrySendable.TrySendable(NotSendable.NotSendable(42), NotSendable.NotSendable(43)))
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingSendable](result)
  }

  test("MissingOrder.01") {
    val input =
      """
        |pub enum E {
        |   case E
        |}
        |
        |def foo(x: E, y: E): Bool = x <= y
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingOrder](result)
  }

  test("MissingToString.01") {
    val input =
      s"""
         |pub enum E {
         |   case E
         |}
         |
         |def foo(x: E): String = ToString.toString(x)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingToString](result)
  }

  test("MissingArrowInstance.01") {
    val input =
      """
        |def main(): Unit \ IO =
        |    println(x -> x + 41i32)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingArrowInstance](result)
  }

  test("TestChoose.Arity1.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        relational_choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        relational_choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        relational_choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.04") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        relational_choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.IfThenElse.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.IfThenElse.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.IfThenElse.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.IfThenElse.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.04") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.ThreeCases.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Present(_))       => 1
        |            case (Present(_), Absent)       => 2
        |            case (Present(_), Present(_))   => 3
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.ThreeCases.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)           => 1
        |            case (Absent, Present(_))       => 2
        |            case (Present(_), Absent)       => 3
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.01") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.02") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.03") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.04") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.05") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.06") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.Choice.Param.01") {
    val input =
      """
        |pub def foo(x: Choice[String, true, _]): Int32 =
        |    relational_choose x {
        |        case Absent => 1
        |    }
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.Choice.Param.02") {
    val input =
      """
        |pub def foo(x: Choice[String, _, true]): Int32 =
        |    relational_choose x {
        |        case Present(_) => 1
        |    }
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.Choice.Empty.01") {
    val input =
      """
        |def foo(): Unit =
        |    let f = x -> {
        |        relational_choose x {
        |            case Absent     => 1
        |        };
        |        relational_choose x {
        |            case Present(_) => 2
        |        }
        |    };
        |    f(Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.Choice.Empty.02") {
    val input =
      """
        |def foo(): Unit =
        |    let f = x -> {
        |        relational_choose x {
        |            case Absent     => 2
        |        };
        |        relational_choose x {
        |            case Present(_) => 2
        |        }
        |    };
        |    f(Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ChooseStar.01") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = x -> {
        |        relational_choose* x {
        |            case Absent     => Absent
        |            case Present(v) => Present(v)
        |        }
        |    };
        |    let isAbsent = x -> relational_choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Present(123)))
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ChooseStar.02") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = x -> {
        |        relational_choose* x {
        |            case Absent     => Present(123)
        |            case Present(_) => Absent
        |        }
        |    };
        |    let isAbsent = x -> relational_choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Absent))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ChooseStar.03") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Eff, _isPresent : Eff] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = (x, y) -> {
        |        relational_choose* (x, y) {
        |            case (Absent, Absent)         => Absent
        |            case (Present(_), Present(_)) => Present(42)
        |        }
        |    };
        |    let isAbsent = x -> relational_choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Present(123), Present(456)))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ImpureDeclaredAsPure.01") {
    val input =
      """
        |pub def f(): Int32 = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ImpureDeclaredAsPure.02") {
    val input =
      """
        |def f(): Int32 \ {} = unchecked_cast(123 as _ \ IO)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.ImpureDeclaredAsPure.03") {
    // Regression test. See https://github.com/flix/flix/issues/4062
    val input =
      """
        |def mkArray(): Array[Int32, Static] \ IO = Array#{} @ Static
        |
        |def zero(): Int32 \ {} = $ARRAY_LENGTH$(mkArray())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.EffectfulDeclaredAsPure.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.EffectfulDeclaredAsPure.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ {} = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.EffectfulDeclaredAsPure.03") {
    val input =
      """
        |eff Print {
        |    pub def print(): Unit
        |}
        |
        |eff Throw {
        |    pub def throw(): Unit
        |}
        |
        |def f(): Unit =
        |    do Print.print();
        |    do Throw.throw()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.EffectGeneralizationError.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef): Int32 \ ef = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.EffectGeneralizationError.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 \ ef1, h: Int32 -> Int32 \ ef2): Int32 \ {ef1, ef2} = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.RegionVarEscapes.01") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = ref 123 @ rc;
        |            x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
  }

  test("Test.RegionVarEscapes.02") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = ref 123 @ rc;
        |            (123, x)
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
  }

  test("Test.RegionVarEscapes.03") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = ref 123 @ rc;
        |            _w -> x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
  }

  test("Test.RegionVarEscapes.04") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region rc {
        |            let x = ref 123 @ rc;
        |            w -> {
        |                discard deref x;
        |                w
        |            }
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
  }

  //  test("Test.RegionVarEscapes.05") {
  //    val input =
  //      """
  //        |pub def g(): Int32 =
  //        |    let region r1;
  //        |    let cell = ref None @ r1;
  //        |    let _ = {
  //        |        let region r2;
  //        |        let x = ref 123 @ r2;
  //        |        cell := Some(_ -> {deref x});
  //        |        ()
  //        |    };
  //        |    42
  //        |
  //      """.stripMargin
  //    val result = compile(input, Options.TestWithLibNix)
  //    expectError[TypeError.RegionVarEscapes](result)
  //    }
  //  }

  test("Test.InvalidOpParamCount.Do.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit \ E = do E.op("hello", "world")
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.InvalidOpParamCount](result)
  }

  test("Test.InvalidOpParamCount.Handler.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit = {
        |    try () with E {
        |        def op(x, y) = ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.InvalidOpParamCount](result)
  }

  test("Test.UnexpectedType.OpParam.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit \ E = do E.op(123)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedType](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.MismatchedEff.Without.01") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def foo(): Unit = do E.op() without E
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.MismatchedEff.Apply.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit = ???
        |
        |def foo(): Unit = disjoint(_ -> do E.op(), _ -> do E.op())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedArrowEffects](result)
  }

  // TODO EFF-MIGRATION temporarily disabled
  ignore("Test.GeneralizationError.Eff.01") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |eff F {
        |    pub def op(): Unit
        |}
        |
        |def doBoth(f: Unit -> Unit \ {ef - E}, g: Unit -> Unit \ {ef - F}): Unit \ {ef - E - F} = g(); f()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
  }

  test("Test.PossibleCheckedTypeCast.01") {
    val input =
      """
        |def f(): ##dev.flix.test.TestClassWithDefaultConstructor \ IO =
        |    import new dev.flix.test.TestClassWithInheritedMethod(): ##dev.flix.test.TestClassWithInheritedMethod as newObj;
        |    let x: ##dev.flix.test.TestClassWithDefaultConstructor = newObj();
        |    x
      """.stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.PossibleCheckedTypeCast](result)
  }

  test("TestParYield.01") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedEffects](result)
  }

  test("TestParYield.02") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (x <- { unchecked_cast(1 as _ \ IO) }) yield x;
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedEffects](result)
  }

  test("TestParYield.03") {
    val input =
      """
        | def f(g: Unit -> Unit \ IO): Unit \ IO =
        |     let _ = par (a <- 1; b <- { unchecked_cast(1 as _ \ IO) }) yield (a, b);
        |     g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedEffects](result)
  }

  test("TestParYield.04") {
    val input =
      """
        | def f(): Int32 =
        |     par (a <- true) yield a + 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedTypes](result)
  }

  test("Test.UnexpectedArgument.01") {
    val input =
      """
        |def f[m: Eff -> Type, a: Eff](_: m[a]): m[a] = ???
        |
        |enum Box[a](a)
        |
        |def g(): Box[Int32] = f(Box.Box(123))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("Test.UnexpectedArgument.02") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def noE(f: Unit -> Unit \ ef - E): Unit = ???
        |
        |def foo(): Unit = noE(_ -> do E.op())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("Test.UnexpectedArgument.03") {
    val input =
      """
        |eff E {
        |    pub def op(): Unit
        |}
        |
        |def mustE(f: Unit -> Unit \ {ef, E}): Unit = ???
        |
        |def foo(): Unit = mustE(x -> x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("Test.UnexpectedArgument.04") {
    val input =
      """
        |def f(x: Bool, y: Bool): Bool = ???
        |
        |law l: forall (x: Int32, y: Bool) . f(x, y)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("Test.UnexpectedArgument.05") {
    // Regression test.
    // See https://github.com/flix/flix/issues/3634
    val input =
    """
      |enum E[a: Type, ef: Eff](Unit)
      |def f(g: E[Int32, Pure]): Bool = ???
      |def mkE(): E[Int32, Pure] \ ef = ???
      |
      |def g(): Bool = f(mkE)
      |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("Test.UnexpectedArgument.06") {
    val input =
      """
        |def takesString(_: String): Unit = ()
        |
        |def f(): Unit = typematch 123 {
        |    case x: _ => takesString(x)
        |    case _: _ => ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedArgument](result)
  }

  test("TestChoose.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = choose Expr.Cst {
        |    case Expr.Var(_) => true
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |     };
        |     let h = if (true) f else g;
        |     h(Expr.Var)
        | }
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let h = if (true) f else g;
        |
        |     let cstOrNotOrVar = if (true) open Expr.Cst else if (true) open Expr.Not else open Expr.Var;
        |
        |     h(cstOrNotOrVar)
        | }
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def quack(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def liquorice(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Not(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.04") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def testChooseStar4(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Var(_) => true
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.05") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Not(_) => Expr.Not()
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.06") {
    val input =
      """
        |restrictable enum E[s] {
        |    case N(E[s])
        |    case C
        |}
        |
        |def n(e: E[s && <E.N>]): _ = ???
        |
        |def foo(e: E[s]): E[s] = choose* e {
        |    case E.N(x) => n(x)            // must have x <: <E.N> but this doesn't hold
        |    case E.C    => E.C
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.01") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Not all cases caught
        |def isRed(c: Color[s]): Bool = choose c {
        |    case Color.Red => true
        |    case Color.Green => false
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.02") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// forgot Green intro
        |def redToGreen(c: Color[s]): Color[s -- <Color.Red>] = choose* c {
        |    case Color.Red => Color.Green
        |    case Color.Green => Color.Green
        |    case Color.Blue => Color.Blue
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.03") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Wrong minus
        |def isRed(c: Color[s -- <Color.Blue>]): Bool = choose* c {
        |    case Color.Red => true
        |    case Color.Blue => false
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestCaseSetAnnotation.04") {
    val input =
      """
        |restrictable enum Color[s] {
        |    case Red, Green, Blue
        |}
        |
        |// Wrong minus parsing
        |def isRed(c: Color[s -- <Color.Red> ++ <Color.Green>]): Color[(s -- <Color.Red>) ++ <Color.Green>] = c
        |""".stripMargin
    expectError[TypeError.MismatchedCaseSets](compile(input, Options.TestWithLibNix))
  }

  test("TestLetRec.01") {
    val input =
      """
        |def f(): Int32 = {
        |    def g(): Bool = 123;
        |    g()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.UnexpectedType](result)
  }
}
