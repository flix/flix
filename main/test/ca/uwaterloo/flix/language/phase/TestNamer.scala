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

  test("DuplicateDefinition.01") {
    val input =
      s"""
         |def f: Int = 42
         |def f: Int = 21
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition.02") {
    val input =
      s"""
         |def f: Int = 42
         |def f: Int = 21
         |def f: Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition.03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition.04") {
    val input =
      s"""
         |def f: Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition.05") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = 42
         |}
         |
         |namespace A {
         |  def f: Int = 21
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition.06") {
    val input =
      s"""
         |namespace A/B/C {
         |  def f: Int = 42
         |}
         |
         |namespace A {
         |  namespace B {
         |    namespace C {
         |      def f: Int = 21
         |    }
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateIndex.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |index R({x})
         |index R({x})
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateIndex](result)
  }

  test("DuplicateIndex.02") {
    val input =
      s"""
         |namespace A {
         |  rel R(x: Int)
         |
         |  index R({x})
         |  index R({x})
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateIndex](result)
  }

  // TODO: DuplicateIndex in different namespaces.
  ignore("DuplicateIndex.03") {
    val input =
      s"""
         |namespace a {
         |  rel R(x: Int)
         |
         |  index R({x})
         |}
         |
         |index a/R({x})
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateIndex](result)
  }

  test("UndefinedNativeClass.01") {
    val input = "def f: Int = unsafe native field java.lang.Foo"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeClass](result)
  }

  test("UndefinedNativeClass.02") {
    val input = "def f: Int = unsafe native method java.lang.Bar.Baz()"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeClass](result)
  }

  test("UndefinedNativeConstructor.01") {
    val input = "def f: Int = unsafe native new java.lang.String(1, 2, 3, 4, 5)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeConstructor](result)
  }

  test("UndefinedNativeConstructor.02") {
    val input = "def f: Int = unsafe native new java.lang.String(1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeConstructor](result)
  }

  test("UndefinedNativeField.01") {
    val input = "def f: Int = unsafe native field java.lang.Math.PIE"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeField](result)
  }

  test("UndefinedNativeField.02") {
    val input = "def f: Int = unsafe native field java.lang.Math.EEE"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeField](result)
  }

  test("UndefinedNativeMethod.01") {
    val input = "def f: Int = unsafe native method java.lang.Math.aaa()"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeMethod](result)
  }

  test("UndefinedNativeMethod.02") {
    val input = "def f: Int = unsafe native method java.lang.Math.bbb(1, 2, 3)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedNativeMethod](result)
  }

  test("AmbiguousNativeConstructor.01") {
    val input = "def f: Int = unsafe native new java.lang.String(42)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeConstructor](result)
  }

  test("AmbiguousNativeConstructor.02") {
    val input = "def f: Int = unsafe native new java.lang.String(42, 84)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeConstructor](result)
  }

  test("AmbiguousNativeMethod.01") {
    val input = "def f: Int = unsafe native method java.lang.Math.abs(1)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeMethod](result)
  }

  test("AmbiguousNativeMethod.02") {
    val input = "def f: Int = unsafe native method java.lang.Math.max(1, 2)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousNativeMethod](result)
  }

}
