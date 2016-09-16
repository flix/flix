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
import ca.uwaterloo.flix.language.errors.NameError
import org.scalatest.FunSuite

class TestNamer extends FunSuite with TestUtils {

  test("DuplicateDefinition01") {
    val input =
      s"""
         |def f: Int = 42
         |def f: Int = 21
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition02") {
    val input =
      s"""
         |def f: Int = 42
         |def f: Int = 21
         |def f: Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition04") {
    val input =
      s"""
         |def f: Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition05") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = 42
         |}
         |
         |namespace A {
         |  def f: Int = 21
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.DuplicateDefinition](result)
  }

  test("DuplicateDefinition06") {
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
    assertError[NameError.DuplicateDefinition](result)
  }

  test("IllegalDefinitionName01") {
    val input = "def F: Int = 42"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalDefinitionName](result)
  }

  test("IllegalDefinitionName02") {
    val input =
      s"""
         |namespace A {
         |  def Foo: Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalDefinitionName](result)
  }

  test("IllegalDefinitionName03") {
    val input =
      s"""
         |namespace A {
         |  def FOO: Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalDefinitionName](result)
  }

  test("IllegalDefinitionName04") {
    val input =
      s"""
         |namespace A {
         |  def F(x: Int): Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalDefinitionName](result)
  }

  test("IllegalRelationName01") {
    val input = "rel f(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }

  test("IllegalRelationName02") {
    val input = "rel foo(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }

  test("IllegalRelationName03") {
    val input = "rel fOO(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }

  test("IllegalLatticeName01") {
    val input = "lat f(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }

  test("IllegalLatticeName02") {
    val input = "lat foo(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }

  test("IllegalLatticeName03") {
    val input = "lat fOO(x: Int)"
    val result = new Flix().addStr(input).compile()
    assertError[NameError.IllegalTableName](result)
  }


  test("UnresolvedEnumReference01") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = Foo.Bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedEnumReference])
    ???
  }

  test("UnresolvedEnumReference02") {
    val input =
      s"""
         |namespace A {
         |  def f: Int = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedEnumReference])
    ???
  }

  test("UnresolvedTagReference01") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f: B = B.Qux;
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
    ???
  }

  test("UnresolvedTagReference02") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f: B = B.Qux(1 + 2);
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
    ???
  }

  test("UnresolvedTagReference03") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(b: B): Int = match b with {
         |    case B.Qux => 42;
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
    ???
  }

  test("UnresolvedRelationReference01") {
    val input =
      s"""namespace A {
          |  VarPointsTo(1, 2).
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedRelationReference])
    ???
  }

  test("UnresolvedRelationReference02") {
    val input =
      s"""namespace A {
          |  VarPointsTo(1, 2).
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedRelationReference])
    ???
  }

  test("UnresolvedTypeReference01") {
    val input = "def x: Foo = 42"
    val result = new Flix().addStr(input).compile()
    // assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTypeReference])
    ???
  }

  test("UnresolvedTypeReference02") {
    val input =
      s"""namespace A {
          |  def foo(bar: Baz, baz: Baz): Qux = bar;
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    //assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTypeReference])
    ???
  }

  test("Expression.Hook01") {
    val input =
      s"""namespace A {
          |  def f(x: Int): Bool = g(x)
          |};
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

  test("Expression.Hook02") {
    val input =
      s"""namespace A {
          |  def f(x: Bool, y: Int, z: Str): Bool = g(x, y, z)
          |};
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

  test("Expression.HookFilter01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str);
          |
          |  R(x, y, z) :- f(x, y, z), R(x, y, z).
          |};
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

  test("Expression.HookApply01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str);
          |
          |  R(x, y, f(x, y, z)) :- R(x, y, z).
          |};
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
