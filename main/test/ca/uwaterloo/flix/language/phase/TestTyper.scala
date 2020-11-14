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
import org.scalatest.FunSuite

class TestTyper extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("TestLeq01") {
    val input =
      """
        |def f(): a = 21
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq02") {
    val input =
      """
        |def f(): List[a] = 21 :: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq03") {
    val input =
      """
        |def f(): Result[a, Int] = Ok(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq04") {
    val input =
      """
        |def f(): Result[Int, a] = Err(21)
        |
        |enum Result[t, e] {
        |    case Ok(t),
        |    case Err(e)
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq05") {
    val input =
      """
        |def f(): a -> a = x -> 21
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq06") {
    val input =
      """
        |def f(): a -> a = (x: Int32) -> x
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq07") {
    val input =
      """
        |def f(): {x: Int | r} = {x = 21}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq08") {
    val input =
      """
        |def f(): {x: Int, y: Int | r} = {y = 42, x = 21}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestOccurs01") {
    val input =
      """
        |rel A(v: Int)
        |rel B(v: Int)
        |
        |def f(a: #{A | r}, b: #{B | r}): #{A, B} = solve (a <+> b)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.OccursCheckError](result)
  }

  test("TestMismatchedKinds.01") {
    val input = "def f(): {| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.02") {
    val input = "def f(): #{| x} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.03") {
    val input = "def f(): {a: Int} = {a = 2} <+> {a = 2}"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.04") {
    val input = "def f(): String = 1 |= 2"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.05") {
    val input = "def f(): String = solve \"hello\""
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.06") {
    val input =
      """
        |rel A(a: Int)
        |
        |def f(): Int = {
        |  let b = "b";
        |  let c = "c";
        |  let d = "d";
        |  fold A b c d
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError](result) // TODO use more specific error once TypeError logic is more defined
  }

  test("TestMismatchedKinds.07") {
    val input =
      """
        |rel A(a: Int)
        |
        |def f(): Int = {
        |  let b = "b";
        |  project A b
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError](result) // TODO use more specific error once TypeError logic is more defined
  }

  test("TestLeq.Wildcard.01") {
    val input = "def f(a: _): _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.02") {
    val input = "def f(a: Int): _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.03") {
    val input = "def f(a: Int): Int & _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.04") {
    val input = "def f(a: Int): Int & _ = a"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Wildcard.05") {
    val input = "def f(g: Int -> Int & _): Int & _ = g(1)"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Class.01") {
    val input =
      """
        |class Show[a] {
        |  def show(x: a): String
        |}
        |def foo(x: a): String = show(x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Class.02") {
    val input =
      """
        |class Show[a] {
        |  def show(x: a): String
        |}
        |def foo(x: Int): String = show(x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Class.03") {
    val input =
      """
        |enum Box[a] {
        |    case Just(a)
        |}
        |
        |class Show[a] {
        |    def show(x: a): String
        |}
        |
        |instance Show[Int] {
        |    def show(x: Int): String = "123"
        |}
        |
        |instance Show[Box[a]] with [a : Show] {
        |    def show(x: Box[a]): String = match x {
        |        case Just(y) => show(y)
        |    }
        |}
        |
        |def doShow(x: Box[Float]): String = show(x)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Class.04") {
    val input =
      """
        |class C[a]
        |
        |enum E[a : C] {
        |    case E1(a)
        |}
        |
        |def f(): E[Int] = E1(1)
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestChoose.Arity1.01") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.02") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.03") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        choose x {
        |            case Absent => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.Arity1.04") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        choose x {
        |            case Present(_) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.03") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.IfThenElse.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentAbsent.IfThenElse.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent) => 1
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.03") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.IfThenElse.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(if (true) Absent else Present(123), Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.AbsentPresent.IfThenElse.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Absent, Present(123))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)         => 1
        |            case (Present(_), Present(_)) => 2
        |        }
        |    };
        |    f(Present(123), Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.03") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.TwoCases.04") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_)) => 1
        |            case (Present(_), Absent) => 2
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.ThreeCases.01") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Present(_))       => 1
        |            case (Present(_), Absent)       => 2
        |            case (Present(_), Present(_))   => 3
        |        }
        |    };
        |    f(Absent, Absent)
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.ThreeCases.02") {
    val input =
      """
        |def main(): Int =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)           => 1
        |            case (Absent, Present(_))       => 2
        |            case (Present(_), Absent)       => 3
        |        }
        |    };
        |    f(Present(123), Present(456))
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.01") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.02") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.03") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (Absent, Absent)    => 1
        |            case (Present(_), Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.04") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Absent, if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.05") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestChoose.If.06") {
    val input =
      """
        |def main(): Bool =
        |    let f = (x, y) -> {
        |        choose (x, y) {
        |            case (_, Absent)    => 1
        |            case (_, Absent)    => 2
        |        }
        |    };
        |    f(if (true) Absent else Present(123), if (true) Absent else Present(456)) == 1
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("TestLeq.Choice.01") {
    val input =
      """
        |pub def f(x: Choice[String, true, _]): Int32 =
        |    choose x {
        |        case Absent => 1
        |    }
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("TestLeq.Choice.02") {
    val input =
      """
        |pub def f(x: Choice[String, _, true]): Int32 =
        |    choose x {
        |        case Present(_) => 1
        |    }
        |
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.GeneralizationError](result)
  }

  test("Test.Choice.Empty.01") {
    val input =
      """
        |def main(): Unit =
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
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.Choice.Empty.02") {
    val input =
      """
        |def main(): Unit =
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
        |pub enum Choice[a, _isAbsent :# Bool, _isPresent :# Bool] {
        |    case Absent
        |    case Present(a)
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedBools](result)
  }

  test("Test.OverlappingInstance.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.OverlappingInstance](result)
  }


  test("Test.OverlappingInstance.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.OverlappingInstance](result)
  }

  test("Test.OverlappingInstance.03") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, Int)]
        |
        |instance C[(Bool, b)]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.OverlappingInstance](result)
  }

  test("Test.MissingImplementation.01") {
    val input =
      """
        |class C[a] {
        |    def get(): a
        |}
        |
        |instance C[Bool] {
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MissingImplementation](result)
  }

  test("Test.MismatchedSignatures.01") {
    val input =
      """
        |class C[a] {
        |    def get(): a
        |}
        |
        |instance C[Bool] {
        |    def get(_i: Int): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedSignatures](result)
  }

  test("Test.ExtraneousDefinition.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Bool] {
        |    def get(): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TypeError.ExtraneousDefinition](result)
  }
}
