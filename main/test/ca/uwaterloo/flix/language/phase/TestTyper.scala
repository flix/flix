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

  // TODO
  ignore("TestLeq.Choice.01") {
    val input =
      """
        |pub def f(x: Choice[String, false, _], y: Choice[String, true, _], z: Choice[String, false, _]): Bool =
        |    choose (x, y, z) {
        |        case (Present(a), Present(b), _) => a == "Hello" && b == "World"
        |        case (_, Present(b), Present(c)) => b == "World" && c == "!"
        |    }
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

  // TODO
  ignore("TestLeq.Choice.02") {
    val input =
      """
        |pub def f(x: Choice[String, true, _], y: Choice[String, false, _], z: Choice[String, true, _]): Bool =
        |    choose (x, y, z) {
        |        case (Present(a), Present(b), _) => a == "Hello" && b == "World"
        |        case (_, Present(b), Present(c)) => b == "World" && c == "!"
        |    }
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

  // TODO
  ignore("TestLeq.Choice.03") {
    val input =
      """
        |def f(x: Choice[String, not not n, _], y: Choice[String, not not n, _]): Bool =
        |    choose (x, y) {
        |        case (Present(a), _) => a == "Hello"
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
    val input = "def f(): Str = 1 |= 2"
    val result = compile(input, DefaultOptions)
    expectError[TypeError.MismatchedKinds](result)
  }

  test("TestMismatchedKinds.05") {
    val input = "def f(): Str = solve \"hello\""
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

}
