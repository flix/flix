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
import ca.uwaterloo.flix.api.{Flix, IValue, Invokable}
import ca.uwaterloo.flix.language.errors.{NameError, ResolutionError, TypeError}
import org.scalatest.FunSuite

class TestNamer extends FunSuite with TestUtils {

  // TODO SPlit into TestNamer and TestResolver

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

  // TODO: Namer
  ignore("UnsafeFact.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO: Namer
  ignore("UnsafeFact.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(42, x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO: Namer
  ignore("UnsafeFact.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(42, x, 21).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO
  ignore("UnsafeRule.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x) :- R(y).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO
  ignore("UnsafeRule.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(x, y) :- R(x, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  // TODO
  ignore("UnsafeRule.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(x, y, z) :- R(x, w, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedRef](result)
  }

  test("AmbiguousTag.01") {
    val input =
      s"""
         |enum A {
         |  case Foo
         |}
         |
         |enum B {
         |  case Foo
         |}
         |
         |def f: A = Foo
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.AmbiguousTag](result)
  }

  test("AmbiguousTag.02") {
    val input =
      s"""
         |enum A {
         |  case Foo(Int)
         |}
         |
         |enum B {
         |  case Foo(Int)
         |}
         |
         |def f: A = Foo(42)
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.AmbiguousTag](result)
  }

  test("UnresolvedEnum.01") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = Foo.Bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UnresolvedEnum.02") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UnresolvedTag.01") {
    val input =
      s"""
         |enum A {
         |  case Foo
         |}
         |
         |def f: A = A.Qux
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UnresolvedTag.02") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f: B = B.Qux(1 + 2)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UnresolvedTag.03") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(b: B): Int = match b with {
         |    case B.Qux => 42
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UnresolvedType.01") {
    val input = "def x: Foo = 42"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UnresolvedType.02") {
    val input =
      s"""namespace A {
         |  def foo(bar: Baz, baz: Baz): Qux = bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedType](result)
  }

  test("Expression.Hook.01") {
    val input =
      s"""namespace A {
         |  def f(x: Int): Bool = g(x)
         |}
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A.g", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    flix.compile().get
  }

  test("Expression.Hook.02") {
    val input =
      s"""namespace A {
         |  def f(x: Bool, y: Int, z: Str): Bool = g(x, y, z)
         |}
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkInt32Type, flix.mkStrType), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A.g", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    flix.compile().get
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
