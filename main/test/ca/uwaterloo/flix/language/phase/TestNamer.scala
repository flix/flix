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


  test("DuplicateTypeAlias.01") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateTypeAlias](result)
  }

  test("DuplicateTypeAlias.02") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateTypeAlias](result)
  }

  test("DuplicateTypeAlias.03") {
    val input =
      s"""
         |namespace A {
         |  type alias USD = Int
         |}
         |
         |namespace A {
         |  type alias USD = Int
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateTypeAlias](result)
  }

  test("UndefinedTypeVar.Def.01") {
    val input = "def f[a](): b = 123"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.02") {
    val input = "def f[a](x: b): Int = 123"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.03") {
    val input = "def f[a, b, c](x: Option[d]): Int = 123"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Rel.01") {
    val input = "rel R[a](x: b)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Rel.02") {
    val input = "rel R[a, b, c](x: a, y: d, z: c)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Lat.01") {
    val input = "lat R[a](x: b)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Lat.02") {
    val input = "lat R[a, b, c](x: a, y: d, z: c)"
    val result = new Flix().addStr(input).compile()
    expectError[NameError.UndefinedTypeVar](result)
  }

}
