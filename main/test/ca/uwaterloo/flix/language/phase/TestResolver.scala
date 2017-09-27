/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.ResolutionError
import org.scalatest.FunSuite

class TestResolver extends FunSuite with TestUtils {

  test("UnsafeFact.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UnsafeFact.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(42, x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UnsafeFact.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(42, x, 21).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UnsafeRule.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x) :- R(y).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UnsafeRule.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(x, y) :- R(x, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UnsafeRule.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(x, y, z) :- R(x, w, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
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
         |def f(): A = Foo
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
         |def f(): A = Foo(42)
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.AmbiguousTag](result)
  }

  test("UndefinedDef.01") {
    val input = "def f(): Int = x"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UndefinedDef.02") {
    val input =
      s"""
         |namespace A {
         |  def f(x: Int, y: Int): Int = x + y + z
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedDef](result)
  }

  test("UndefinedTag.01") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = Foo.Bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.02") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.03") {
    val input =
      s"""
         |enum A {
         |  case Foo
         |}
         |
         |def f(): A = A.Qux
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.04") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(): B = B.Qux(1 + 2)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.05") {
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

  test("UndefinedTable.01") {
    val input = "VarPointsTo(1, 2)."
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

  test("UndefinedTable.02") {
    val input =
      s"""namespace A {
         |  VarPointsTo(1, 2).
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

  test("UndefinedTable.03") {
    val input = "index AddrOf({foo, bar})"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedTable](result)
  }

  test("UndefinedType.01") {
    val input = "def x(): Foo = 42"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.02") {
    val input =
      s"""namespace A {
         |  def foo(bar: Baz, baz: Baz): Qux = bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedType](result)
  }

}
