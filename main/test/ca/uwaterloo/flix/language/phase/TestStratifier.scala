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
import org.scalatest.FunSuite

class TestStratifier extends FunSuite with TestUtils {

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Stratification.01") {
    val input =
      """
        |rel A(c: Int)
        |rel X(c: Int)
        |A(c) :- X(c), !A(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.02") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel X(c: Int)
        |A(c) :- X(c), B(c).
        |B(c) :- X(c), !A(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.03") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel X(c: Int)
        |A(c) :- X(c), !B(c).
        |B(c) :- X(c), A(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.04") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel C(c: Int)
        |rel D(c: Int)
        |rel E(c: Int)
        |rel F(c: Int)
        |rel G(c: Int)
        |rel H(c: Int)
        |rel I(c: Int)
        |rel J(c: Int)
        |rel K(c: Int)
        |rel X(c: Int)
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
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.05") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel C(c: Int)
        |rel X(c: Int)
        |C(c) :- X(c), !A(c).
        |A(c) :- B(c), C(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.06") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel X(c: Int)
        |A(c) :- X(c), !A(c).
        |B(c) :- X(c), !B(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.07") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |rel C(c: Int)
        |rel D(c: Int)
        |rel X(c: Int)
        |A(c) :- X(c), B(c).
        |B(c) :- X(c), C(c).
        |C(c) :- X(c), !A(c).
        |B(c) :- X(c), D(c).
        |D(c) :- X(c), !A(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }
}
