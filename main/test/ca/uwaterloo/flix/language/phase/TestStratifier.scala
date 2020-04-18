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

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Stratification.01") {
    val input =
      """
        |A(c) :- X(c), !A(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.02") {
    val input =
      """
        |A(c) :- X(c), B(c).
        |B(c) :- X(c), !A(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.03") {
    val input =
      """
        |A(c) :- X(c), !B(c).
        |B(c) :- X(c), A(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.04") {
    val input =
      """
        |A(c) :- B(c).
        |B(c) :- C(c).
        |C(c) :- D(c).
        |D(c) :- E(c).
        |E(c) :- F(c).
        |F(c) :- G(c).
        |G(c) :- H(c).
        |H(c) :- I(c).
        |I(c) :- J(c).
        |J(c) :- K(c).
        |K(c) :- X(c), !A(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.05") {
    val input =
      """
        |C(c) :- X(c), !A(c).
        |A(c) :- B(c), C(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.06") {
    val input =
      """
        |A(c) :- X(c), !A(c).
        |B(c) :- X(c), !B(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

  test("Stratification.07") {
    val input =
      """
        |A(c) :- X(c), B(c).
        |B(c) :- X(c), C(c).
        |C(c) :- X(c), !A(c).
        |B(c) :- X(c), D(c).
        |D(c) :- X(c), !A(c).
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[StratificationError](result)
  }

}
