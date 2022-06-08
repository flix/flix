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
import ca.uwaterloo.flix.util.{Formatter, Options}
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  test("TestLeq01") {
    val input =
      """
        |def foo(): a = 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
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
  }

  test("TestLeq05") {
    val input =
      """
        |def foo(): a -> a = x -> 21
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestLeq06") {
    val input =
      """
        |def foo(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestLeq07") {
    val input =
      """
        |def foo(): {x :: Int32 | r} = {x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestLeq08") {
    val input =
      """
        |def foo(): {x :: Int32, y :: Int32 | r} = {y = 42, x = 21}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestOccurs01") {
    val input =
      """
        |rel A(v: Int32)
        |rel B(v: Int32)
        |
        |def foo(a: #{A | r}, b: #{B | r}): #{A, B} = solve (a <+> b)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.OccursCheckError](result)
    }
  }

  test("TestMismatchedTypes.01") {
    val input = "def foo(): {| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
  }

  test("TestMismatchedTypes.02") {
    val input = "def foo(): #{| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
  }

  test("TestMismatchedTypes.03") {
    val input = "def foo(): {a :: Int32} = {a = 2} <+> {a = 2}"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
  }

  test("TestMismatchedTypes.04") {
    val input = "def foo(): String = solve \"hello\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
  }

  test("TestMismatchedTypes.Arrow.01") {
    // Regression test.
    // See https://github.com/flix/flix/issues/3634
    val input =
      """
        |enum E[a: Type, ef: Bool](Unit)
        |def f(g: E[Int32, true]): Bool = ???
        |def mkE(): E[Int32, true] & ef = ???
        |
        |def g(): Bool = f(mkE)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
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
  }

  test("TestLeq.Wildcard.01") {
    val input = "def foo(a: _): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestLeq.Wildcard.02") {
    val input = "def foo(a: Int32): _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
  }

  test("TestLeq.Wildcard.03") {
    val input = "def foo(a: Int32): Int32 & _ = a"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectGeneralizationError](result)
    }
  }

  test("TestLeq.Wildcard.05") {
    val input = "def foo(g: Int32 -> Int32 & _): Int32 & _ = g(1)"
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.GeneralizationError](result)
    }
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
        |        case Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Float64]): String = C.foo(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
    }
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
        |        case Box(y) => C.foo(y)
        |    }
        |}
        |
        |def doF(x: Box[Int32]): String = C.foo(C.foo(x))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
    }
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
  }

  test("NoMatchingInstance.07") {
    val input =
      """
        |class C[a] {
        |    pub def foo(x: a): Int32
        |}
        |
        |enum E[_: Bool] {
        |    case E(Int32)
        |}
        |
        |instance C[E[true]] {
        |    pub def foo(x: E[true]): Int32 = 1
        |}
        |
        |def bar(): Int32 = C.foo(E(123))    // E(123) has type E[_], not E[true]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MissingInstance](result)
    }
  }

  test("NoMatchingInstance.Relation.01") {
    val input =
      """
        |pub enum E {
        |   case E1
        |}
        |
        |rel R(e: E)
        |
        |pub def f(): Bool = {
        |   let _x = #{
        |     R(E1).
        |   };
        |   true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingInstance](result)
    }
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
  }

  test("MissingArrowInstance.01") {
    val input =
      s"""
         |def main(): Unit & Impure =
         |    println(x -> x + 41i32)
         |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MissingArrowInstance](result)
    }
  }

  test("TestChoose.Arity1.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.Arity1.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.Arity1.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.Arity1.04") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = x -> {
        |        choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentAbsent.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentAbsent.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentAbsent.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentAbsent.IfThenElse.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentAbsent.IfThenElse.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentPresent.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentPresent.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentPresent.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentPresent.IfThenElse.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.AbsentPresent.IfThenElse.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.TwoCases.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.TwoCases.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.TwoCases.03") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.TwoCases.04") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.ThreeCases.01") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_))       => 1
        |            case (Present(_), Absent)       => 2
        |            case (Present(_), Present(_))   => 3
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.ThreeCases.02") {
    val input =
      """
        |def foo(): Int32 =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)           => 1
        |            case (Absent, Present(_))       => 2
        |            case (Present(_), Absent)       => 3
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.01") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.02") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.03") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.04") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.05") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("TestChoose.If.06") {
    val input =
      """
        |def foo(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("Test.Choice.Param.01") {
    val input =
      """
        |pub def foo(x: Choice[String, true, _]): Int32 =
        |    choose x {
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
  }

  test("Test.Choice.Param.02") {
    val input =
      """
        |pub def foo(x: Choice[String, _, true]): Int32 =
        |    choose x {
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
  }

  test("Test.Choice.Empty.01") {
    val input =
      """
        |def foo(): Unit =
        |    let f = x -> {
        |        choose x {
        |            case Absent     => 1 as & Impure
        |        };
        |        choose x {
        |            case Present(_) => 2 as & Impure
        |        }
        |    };
        |    f(Absent)
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
  }

  test("Test.Choice.Empty.02") {
    val input =
      """
        |def foo(): Unit =
        |    let f = x -> {
        |        choose x {
        |            case Absent     => 1 as & Impure
        |        };
        |        choose x {
        |            case Present(_) => 2 as & Impure
        |        }
        |    };
        |    f(Present(123))
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
  }

  test("Test.MismatchedTypes.Law.01") {
    val input =
      """
        |def f(x: Bool, y: Bool): Bool = ???
        |
        |law l: forall (x: Int32, y: Bool) . f(x, y)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedTypes](result)
    }
  }

  test("Test.ChooseStar.01") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = x -> {
        |        choose* x {
        |            case Absent     => Absent
        |            case Present(v) => Present(v)
        |        }
        |    };
        |    let isAbsent = x -> choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Present(123)))
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("Test.ChooseStar.02") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = x -> {
        |        choose* x {
        |            case Absent     => Present(123)
        |            case Present(_) => Absent
        |        }
        |    };
        |    let isAbsent = x -> choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Absent))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("Test.ChooseStar.03") {
    val input =
      """
        |pub enum Choice[a : Type, _isAbsent : Bool, _isPresent : Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |pub def f(): Bool =
        |    let f = (x, y) -> {
        |        choose* (x, y) {
        |            case (Absent, Absent)         => Absent
        |            case (Present(_), Present(_)) => Present(42)
        |        }
        |    };
        |    let isAbsent = x -> choose x {
        |        case Absent => true
        |    };
        |    isAbsent(f(Present(123), Present(456)))
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.MismatchedBools](result)
    }
  }

  test("Test.ImpureDeclaredAsPure.01") {
    val input =
      """
        |pub def f(): Int32 = 123 as & Impure
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.ImpureDeclaredAsPure](result)
    }
  }

  test("Test.ImpureDeclaredAsPure.02") {
    val input =
      """
        |def f(): Int32 & Pure = 123 as & Impure
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.ImpureDeclaredAsPure](result)
    }
  }

  test("Test.EffectPolymorphicDeclaredAsPure.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 & ef): Int32 = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectPolymorphicDeclaredAsPure](result)
    }
  }

  test("Test.EffectPolymorphicDeclaredAsPure.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 & ef): Int32 & Pure = g(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectPolymorphicDeclaredAsPure](result)
    }
  }

  test("Test.EffectGeneralizationError.01") {
    val input =
      """
        |def f(g: Int32 -> Int32 & ef): Int32 & ef = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectGeneralizationError](result)
    }
  }

  test("Test.EffectGeneralizationError.02") {
    val input =
      """
        |def f(g: Int32 -> Int32 & ef1, h: Int32 -> Int32 & ef2): Int32 & (ef1 and ef2) = 123
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.EffectGeneralizationError](result)
    }
  }

  test("Test.RegionVarEscapes.01") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region r {
        |            let x = ref 123 @ r;
        |            x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
    }
  }

  test("Test.RegionVarEscapes.02") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region r {
        |            let x = ref 123 @ r;
        |            (123, x)
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
    }
  }

  test("Test.RegionVarEscapes.03") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region r {
        |            let x = ref 123 @ r;
        |            _w -> x
        |        }
        |    };
        |    42
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError.RegionVarEscapes](result)
    }
  }

  test("Test.RegionVarEscapes.04") {
    val input =
      """
        |pub def f(): Int32 =
        |    let _ = {
        |        region r {
        |            let x = ref 123 @ r;
        |            w -> {
        |                let _ = deref x;
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

}
