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

  test("AmbiguousVarOrUse.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    f(123)
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousVarOrUse](result)
  }

  test("AmbiguousVarOrUse.02") {
    val input =
      s"""
         |def main(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    use Foo.g;
         |    let g = _ -> true;
         |    f(g(123))
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.AmbiguousVarOrUse](result)
  }

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

  test("DuplicateUseDef.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use A.f;
         |    use B.f;
         |    f() == f()
         |
         |namespace A {
         |    def f(): Int = 1
         |}
         |
         |namespace B {
         |    def f(): Int = 1
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.02") {
    val input =
      s"""
         |use A.f;
         |use B.f;
         |
         |def main(): Bool =
         |    f() == f()
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
         |
         |namespace B {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.03") {
    val input =
      s"""
         |use A.f;
         |
         |def main(): Bool =
         |    use B.f;
         |    f() == f()
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
         |
         |namespace B {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.04") {
    val input =
      s"""
         |def main(): Bool =
         |    use A.{f => g, f => g};
         |    g() == g()
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseTyp.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use A.Color;
         |    use B.Color;
         |    true
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
         |
         |namespace B {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTyp](result)
  }

  test("DuplicateUseTyp.02") {
    val input =
      s"""
         |use A.Color;
         |use B.Color;
         |
         |def main(): Bool = true
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
         |
         |namespace B {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTyp](result)
  }


  test("DuplicateUseTag.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use A.Color.Red;
         |    use B.Color.Red;
         |    Red == Red
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
         |namespace B {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.02") {
    val input =
      s"""
         |use A.Color.Red;
         |use B.Color.Red;
         |def main(): Bool =
         |    Red == Red
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
         |namespace B {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.03") {
    val input =
      s"""
         |
         |use A.Color.Red;
         |def main(): Bool =
         |    use B.Color.Red;
         |    Red == Red
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
         |namespace B {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.04") {
    val input =
      s"""
         |def main(): Bool =
         |    use B.Color.{Red => R};
         |    use B.Color.{Blu => R};
         |    R == R
         |
         |namespace A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.DuplicateUseTag](result)
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

  test("SuspiciousTypeVarName.01") {
    val input =
      s"""
         |def f(_x: List[unit]): Unit = ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.02") {
    val input =
      s"""
         |def f(_x: List[Result[Unit, bool]]): Unit = ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.03") {
    val input =
      s"""
         |def f(): List[char] = ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.04") {
    val input =
      s"""
         |def f(): Unit =
         |    let x: int = 42;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.05") {
    val input =
      s"""
         |enum A {
         |    case X(string)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[NameError.SuspiciousTypeVarName](result)
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
