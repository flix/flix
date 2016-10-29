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
         |namespace A.B.C {
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
         |namespace a {
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

  test("UnsafeFact.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[TypeError.UnresolvedRef](result)
  }

  test("UnsafeFact.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(42, x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[TypeError.UnresolvedRef](result)
  }

  test("UnsafeFact.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(42, x, 21).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[TypeError.UnresolvedRef](result)
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
    expectError[TypeError.UnresolvedRef](result)
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
    expectError[TypeError.UnresolvedRef](result)
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
    expectError[TypeError.UnresolvedRef](result)
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
    expectError[TypeError.UnresolvedType](result)
  }

  test("UnresolvedType.02") {
    val input =
      s"""namespace A {
          |  def foo(bar: Baz, baz: Baz): Qux = bar
          |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[TypeError.UnresolvedType](result)
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
      .addHook("A/g", tpe, new Invokable {
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
      .addHook("A/g", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    flix.compile().get
  }

  test("Expression.HookFilter.01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str)
          |
          |  R(x, y, z) :- f(x, y, z), R(x, y, z).
          |}
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkInt32Type, flix.mkStrType), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A/f", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    val result = flix.compile()
    assert(result.isSuccess)
  }

  test("Expression.HookApply.01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str)
          |
          |  R(x, y, f(x, y, z)) :- R(x, y, z).
          |}
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkStrType), flix.mkStrType)
    flix
      .addStr(input)
      .addHook("A/f", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkStr("foo")
      })
    val result = flix.compile()
    assert(result.isSuccess)
  }

}
