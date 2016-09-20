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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.errors.WeederError
import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  test("DuplicateAlias.01") {
    val input = "P(x, y) :- A(x), y := 21, y := 42. "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAlias])
  }

  test("DuplicateAlias.02") {
    val input = "P(x, y) :- A(x), y := 21, z := 84, y := 42."
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAlias])
  }

  test("DuplicateAnnotation.01") {
    val input =
      """@strict @strict
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAnnotation])
  }

  test("DuplicateAnnotation.02") {
    val input =
      """@strict @monotone @strict @monotone
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAnnotation])
  }

  test("DuplicateAttribute.01") {
    val input = "rel A(x: Int, x: Int)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute.02") {
    val input = "rel A(x: Int, y: Int, x: Int)   "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute.03") {
    val input = "rel A(x: Bool, x: Int, x: Str)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateAttribute])
  }

  test("DuplicateFormal.01") {
    val input = "def f(x: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateFormal])
  }

  test("DuplicateFormal.02") {
    val input = "def f(x: Int, y: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateFormal])
  }

  test("DuplicateFormal.03") {
    val input = "def f(x: Bool, x: Int, x: Str): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateFormal])
  }

  test("DuplicateFormal.04") {
    val input = "def f: Bool = ∀(x: E, x: E). true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateFormal])
  }

  test("DuplicateFormal.05") {
    val input = "def f: Bool = ∃(x: E, x: E). true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateFormal])
  }

  test("DuplicateTag.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateTag])
  }

  test("DuplicateTag.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.DuplicateTag])
  }

  test("EmptyIndex.01") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A()
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.EmptyIndex])
  }

  test("EmptyRelation.01") {
    val input = "rel R()"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.EmptyRelation])
  }

  test("EmptyLattice.01") {
    val input = "lat L()"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.EmptyLattice])
  }

  test("IllegalAnnotation.01") {
    val input =
      """@abc
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalAnnotation])
  }

  test("IllegalAnnotation.02") {
    val input =
      """@foobarbaz
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalAnnotation])
  }

  test("IllegalExistential.01") {
    val input = "def f: Prop = ∃. true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalExistential])
  }

  test("IllegalExistential.02") {
    val input = "def f: Prop = ∃(). true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalExistential])
  }

  test("IllegalIndex.01") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A({})
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalIndex])
  }

  test("IllegalIndex.02") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A({x}, {}, {z})
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalIndex])
  }

  test("IllegalHeadPredicate.Alias.01") {
    val input = "x := y."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.Alias.02") {
    val input = "x := y :- A(x, y)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.NotEqual.01") {
    val input = "x != y."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.NotEqual.02") {
    val input = "x != y :- A(x, y)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalHeadPredicate])
  }

  test("IllegalInt8.01") {
    val input = "def f: Int8 = -1000i8"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt8.02") {
    val input = "def f: Int8 = 1000i8"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt16.01") {
    val input = "def f: Int16 = -100000i16"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt16.02") {
    val input = "def f: Int16 = 100000i16"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt32.01") {
    val input = "def f: Int32 = -10000000000i32"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt32.02") {
    val input = "def f: Int32 = 10000000000i32"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt64.01") {
    val input = "def f: Int64 = -100000000000000000000i64"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalInt64.02") {
    val input = "def f: Int64 = 100000000000000000000i64"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalInt])
  }

  test("IllegalLattice.01") {
    val input = "let Foo<> = (1, 2)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalLattice])
  }

  test("IllegalLattice.02") {
    val input = "let Foo<> = (1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalLattice])
  }

  test("IllegalParameterList.01") {
    val input = "def f(): Int = 42"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalParameterList])
  }

  test("IllegalUniversal.01") {
    val input = "def f: Prop = ∀. true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalUniversal])
  }

  test("IllegalUniversal.02") {
    val input = "def f: Prop = ∀(). true"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalUniversal])
  }

  test("IllegalWildcard.01") {
    val input = "def f: Int = _"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalWildcard])
  }

  test("IllegalWildcard.02") {
    val input = "def f: Int = 42 + _"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalWildcard])
  }

  test("IllegalWildcard.03") {
    val input = "def f: Set[Int] = #{1, 2, _}"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[WeederError.IllegalWildcard])
  }

  test("NonLinearPattern.01") {
    val input =
      """def f: Bool = match (21, 42) with {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.NonLinearPattern])
  }

  test("NonLinearPattern.02") {
    val input =
      """def f: Bool = match (21, 42, 84) with {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.NonLinearPattern])
  }

  test("NonLinearPattern.03") {
    val input =
      """def f: Bool = match (1, (2, (3, 4))) with {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[WeederError.NonLinearPattern])
  }

}
