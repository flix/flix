/*
 * Copyright 2015-2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.errors.NameError
import org.scalatest.FunSuite

class TestNamer extends FunSuite with TestUtils {

  test("DuplicateDef.01") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.02") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
         |def f(): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.04") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.05") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  def f(): Int = 21
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.06") {
    val input =
      s"""
         |namespace A/B/C {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  namespace B {
         |    namespace C {
         |      def f(): Int = 21
         |    }
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateEff.01") {
    val input =
      s"""
         |eff f(): Int
         |eff f(): Int
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateEff](result)
  }

  test("DuplicateEff.02") {
    val input =
      s"""
         |namespace A {
         |  eff f(): Int
         |}
         |
         |namespace A {
         |  eff f(): Int
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateEff](result)
  }

  test("DuplicateHandler.01") {
    val input =
      s"""
         |handler f(): Int = 42
         |handler f(): Int = 21
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateHandler](result)
  }

  test("DuplicateHandler.02") {
    val input =
      s"""
         |namespace A {
         |  handler f(): Int = 42
         |}
         |
         |namespace A {
         |  handler f(): Int = 21
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateHandler](result)
  }

  test("DuplicateClass.01") {
    val input =
      s"""
         |class X[a]
         |class X[a]
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateClass](result)
  }

  test("DuplicateClass.02") {
    val input =
      s"""
         |namespace A {
         |  class X[a]
         |  class X[a]
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateClass](result)
  }

  test("DuplicateClass.03") {
    val input =
      s"""
         |namespace A {
         |  class X[a]
         |}
         |
         |namespace A {
         |  class X[a]
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateClass](result)
  }

  test("ShadowedVar.Let.01") {
    val input =
      """
        |def main(): Int =
        |    let x = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Let.02") {
    val input =
      """
        |def main(): Int =
        |    let x = 123;
        |    let y = 456;
        |    let x = 789;
        |    x
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Def.01") {
    val input =
      """
        |def f(x: Int): Int =
        |    let x = 123;
        |    x
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Def.02") {
    val input =
      """
        |def f(x: Int): Int =
        |    let y = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.01") {
    val input =
      """
        |def main(): Int =
        |    let x = 123;
        |    let f = x -> x + 1;
        |    f(x)
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.02") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        let x = 456;
        |        x + 1
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.03") {
    val input =
      """
        |def main(): Int =
        |    let f = x -> {
        |        let g = x -> 123;
        |        g(456)
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Existential.01") {
    val input =
      """
        |def main(): Bool =
        |    let x = 123;
        |    \exists (x: Int). x == 0
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("ShadowedVar.Universal.01") {
    val input =
      """
        |def main(): Bool =
        |    let x = 123;
        |    \forall (x: Int). x == 0
        |
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.ShadowedVar](result)
  }

  test("UndefinedNativeClass.01") {
    val input = "def f(): Int = native field java.lang.Foo"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeClass](result)
  }

  test("UndefinedNativeClass.02") {
    val input = "def f(): Int = native method java.lang.Bar.Baz()"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeClass](result)
  }

  test("UndefinedNativeConstructor.01") {
    val input = "def f(): Int = native new java.lang.String(1, 2, 3, 4, 5)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeConstructor](result)
  }

  test("UndefinedNativeConstructor.02") {
    val input = "def f(): Int = native new java.lang.String(1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeConstructor](result)
  }

  test("UndefinedNativeField.01") {
    val input = "def f(): Int = native field java.lang.Math.PIE"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeField](result)
  }

  test("UndefinedNativeField.02") {
    val input = "def f(): Int = native field java.lang.Math.EEE"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeField](result)
  }

  test("UndefinedNativeMethod.01") {
    val input = "def f(): Int = native method java.lang.Math.aaa()"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeMethod](result)
  }

  test("UndefinedNativeMethod.02") {
    val input = "def f(): Int = native method java.lang.Math.bbb(1, 2, 3)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeMethod](result)
  }

  test("AmbiguousNativeConstructor.01") {
    val input = "def f(): Int = native new java.lang.String(42)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeConstructor](result)
  }

  test("AmbiguousNativeConstructor.02") {
    val input = "def f(): Int = native new java.lang.String(42, 84)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeConstructor](result)
  }

  test("AmbiguousNativeMethod.01") {
    val input = "def f(): Int = native method java.lang.Math.abs(1)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeMethod](result)
  }

  test("AmbiguousNativeMethod.02") {
    val input = "def f(): Int = native method java.lang.Math.max(1, 2)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeMethod](result)
  }

}
