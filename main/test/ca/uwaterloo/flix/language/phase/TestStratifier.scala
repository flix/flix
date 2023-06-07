/*
 * Copyright 2017 Jason Mittertreiner
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
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestStratifier extends AnyFunSuite with TestUtils {

  val DefaultOptions: Options = Options.TestWithLibMin

  test("Stratification.01") {
    val input =
      """
        |pub def f(): #{A(String), X(String)} = #{
        |  A(c: String) :- X(c), not A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.02") {
    val input =
      """
        |pub def f(): #{A(String), B(String), X(String)} = #{
        |  A(c: String) :- X(c), B(c).
        |  B(c: String) :- X(c), not A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.03") {
    val input =
      """
        |pub def f(): #{A(String), B(String), X(String)} = #{
        |  A(c: String) :- X(c), not B(c).
        |  B(c: String) :- X(c), A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.04") {
    val input =
      """
        |pub def f(): #{A(String), B(String), C(String), D(String), E(String), F(String), G(String), H(String), I(String), J(String), K(String), X(String)} = #{
        |  A(c: String) :- B(c).
        |  B(c: String) :- C(c).
        |  C(c: String) :- D(c).
        |  D(c: String) :- E(c).
        |  E(c: String) :- F(c).
        |  F(c: String) :- G(c).
        |  G(c: String) :- H(c).
        |  H(c) :- I(c).
        |  I(c: String) :- J(c).
        |  J(c: String) :- K(c).
        |  K(c: String) :- X(c), not A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.05") {
    val input =
      """
        |pub def f(): #{A(t), B(t), C(t), X(t)} with Order[t] = #{
        |  C(c) :- X(c), not A(c).
        |  A(c) :- B(c), C(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.06") {
    val input =
      """
        |pub def f(): #{A(t), B(t), X(t)} with Order[t] = #{
        |  A(c) :- X(c), not A(c).
        |  B(c) :- X(c), not B(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.07") {
    val input =
      """
        |pub def f(): #{A(String), B(String), C(String), D(String), X(String)} = #{
        |  A(c: String) :- X(c), B(c).
        |  B(c: String) :- X(c), C(c).
        |  C(c: String) :- X(c), not A(c).
        |  B(c: String) :- X(c), D(c).
        |  D(c: String) :- X(c), not A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.08") {
    // stratification error:
    // ... <- A <- B <-x A <- ...
    val input =
    """
      |pub enum A(Int32)
      |
      |instance Eq[A] {
      |  pub def eq(_a1: A, _a2: A): Bool =
      |    let p1 = #{
      |        B301(12).
      |        A301(x) :- B301(x).
      |    };
      |    let p2 = #{
      |        A301(15). C301(15).
      |        B301(x) :- not A301(x), C301(x).
      |    };
      |    (query p1 select x from A301(x) |> Vector.length) +
      |    (query p2 select x from B301(x) |> Vector.length)
      |    > 0
      |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.09") {
    val input =
      """
        |pub def f(): Bool =
        |    fHelper(12)
        |
        |def fHelper(v: v): Bool with Order[v] =
        |    let p1 = #{
        |        B302(Some(v)).
        |        A302(x) :- B302(x).
        |    };
        |    let p2 = #{
        |        A302(Some("hey")). C302(Some("heyy")).
        |        B302(x) :- not A302(x), C302(x).
        |    };
        |    (query p1 select x from A302(x) |> Vector.length) +
        |    (query p2 select x from B302(x) |> Vector.length)
        |    > 0
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.10") {
    val input =
      """
        |pub def f(): #{A(String), X(String)} = #{
        |  A(c: String) :- X(c), fix A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.11") {
    val input =
      """
        |pub def f(): #{A(String), B(String), X(String)} = #{
        |  A(c: String) :- X(c), B(c).
        |  B(c: String) :- X(c), fix A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.12") {
    val input =
      """
        |pub def f(): #{A(String), B(String), C(String), T(String)} = #{
        |  T("hey").
        |  A(c: String) :- not B(c), T(c).
        |  B(c: String) :- fix C(c).
        |  C(c: String) :- A(c).
        |}
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.13") {
    val input =
      """
        |pub def f(): #{A(String), B(String), C(String; Int32), T(String)} = #{
        |  T("hey").
        |  A(c: String) :- fix B(c), T(c).
        |  B(c: String) :- fix C(c; 12).
        |  C(c: String; 12) :- A(c).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.14") {
    val input =
      """
        |pub def f(): #{A(String, Int32), B(String; Int32), C(String, Int32), D(String; Int32)} = #{
        |  A(c: String, x: Int32) :- fix B(c; x).
        |  B(c; x) :- C(c, x).
        |  C(c, x) :- fix D(c; x).
        |  D(c; x) :- B(c; x).
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.15") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- fix A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.16") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- A(2; 3), fix A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.17") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- fix A(2; 3), A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.18") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- not A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.19") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- A(2; 3), not A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

  test("Stratification.20") {
    val input =
      """
        |pub def f(): Int32 =
        |    let p = #{
        |        A(1; 2) :- not A(2; 3), A(2; 3).
        |    };
        |    let ans = query p select (a, b) from A(a; b);
        |    Vector.length(ans)
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[StratificationError](result)
  }

}
