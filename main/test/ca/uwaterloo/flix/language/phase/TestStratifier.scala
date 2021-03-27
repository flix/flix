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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestStratifier extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

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
        |pub def f[t has Eq has Hash has ToString](): #{A(t), B(t), C(t), X(t)} = #{
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
        |pub def f[t has Eq has Hash has ToString](): #{A(t), B(t), X(t)} = #{
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

}
