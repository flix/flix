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
        |rel Foo(c: Int)
        |Foo(c) :- !Foo(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.02") {
    val input =
      """
        |rel Foo(c: Int)
        |rel Bar(c: Int)
        |Foo(c) :- Bar(c).
        |Bar(c) :- !Foo(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.03") {
    val input =
      """
        |rel Foo(c: Int)
        |rel Bar(c: Int)
        |Foo(c) :- !Bar(c).
        |Bar(c) :- Foo(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.04") {
    val input =
      """
        |rel Foo(c: Int)
        |rel Foo1(c: Int)
        |rel Foo2(c: Int)
        |rel Foo3(c: Int)
        |rel Foo4(c: Int)
        |rel Foo5(c: Int)
        |rel Foo6(c: Int)
        |rel Foo7(c: Int)
        |rel Foo8(c: Int)
        |rel Foo9(c: Int)
        |rel Foo10(c: Int)
        |Foo(c) :- Foo1(c).
        |Foo1(c) :- Foo2(c).
        |Foo2(c) :- Foo3(c).
        |Foo3(c) :- Foo4(c).
        |Foo4(c) :- Foo5(c).
        |Foo5(c) :- Foo6(c).
        |Foo6(c) :- Foo7(c).
        |Foo7(c) :- Foo8(c).
        |Foo8(c) :- Foo9(c).
        |Foo9(c) :- Foo10(c).
        |Foo10(c) :- !Foo(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.05") {
    val input =
      """
        |rel Foo(c: Int)
        |rel Bar(c: Int)
        |rel Baz(c: Int)
        |Baz(c) :- !Foo(c).
        |Foo(c) :- Bar(c), Baz(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }

  test("Stratification.06") {
    val input =
      """
        |rel A(c: Int)
        |rel B(c: Int)
        |A(c) :- !A(c).
        |B(c) :- !B(c).
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
        |rel E(c: Int)
        |A(c) :- B(c).
        |B(c) :- C(c).
        |C(c) :- !A(c).
        |B(c) :- D(c).
        |D(c) :- !A(c).
      """.stripMargin
    expectError[StratificationError](new Flix().addStr(input).compile())
  }
}
