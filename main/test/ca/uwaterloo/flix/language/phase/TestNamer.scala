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
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestNamer extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("AmbiguousVarOrUse.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    f(123)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.AmbiguousVarOrUse](result)
  }

  test("DuplicateDef.01") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.02") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
         |def f(): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDef](result)
  }

  test("DuplicateDef.04") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDef](result)
  }


  test("DuplicateUseDef.05") {
    val input =
      s"""
         |namespace T {
         |    def main(): Bool =
         |        use A.f;
         |        use B.f;
         |        f() == f()
         |}
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
         |
         |namespace B {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.06") {
    val input =
      s"""
         |use A.f;
         |
         |namespace T {
         |    use B.f;
         |    def main(): Bool =
         |        f() == f()
         |}
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
         |
         |namespace B {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.07") {
    val input =
      s"""
         |namespace T {
         |    use A.{f => g, f => g};
         |    def main(): Bool =
         |        g() == g()
         |}
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDef](result)
  }

  test("DuplicateUseDef.08") {
    val input =
      s"""
         |namespace T {
         |    use A.f;
         |    def main(): Bool =
         |        use B.f;
         |        f() == f()
         |}
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
         |
         |namespace B {
         |    pub def f(): Int = 1
         |}
         |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseTyp](result)
  }

  test("DuplicateUseTyp.03") {
    val input =
      s"""
         |namespace T {
         |    use A.Color;
         |    use B.Color;
         |    def main(): Bool =
         |        true
         |}
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.05") {
    val input =
      s"""
         |namespace T {
         |    use A.Color.Red;
         |    use B.Color.Red;
         |    def main(): Bool =
         |        Red == Red
         |}
         |
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.06") {
    val input =
      s"""
         |namespace T {
         |    use A.Color.Red;
         |    def main(): Bool =
         |        use B.Color.Red;
         |        Red == Red
         |}
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseTag.07") {
    val input =
      s"""
         |namespace T {
         |    use B.Color.{Red => R};
         |    use B.Color.{Blu => R};
         |    def main(): Bool =
         |        R == R
         |}
         |namespace A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUseClass.01") {
    val input =
      s"""
         |def main(): Bool =
         |    use class A.Show;
         |    use class B.Show;
         |    true
         |
         |namespace A {
         |    class Show[a] {
         |        def show(x: a): String
         |    }
         |}
         |
         |namespace B {
         |    class Show[a] {
         |        def show(x: a): String
         |    }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseClass](result)
  }

  test("DuplicateUseClass.02") {
    val input =
      s"""
         |use class A.Show;
         |use class B.Show;
         |
         |def main(): Bool = true
         |
         |namespace A {
         |    class Show[a] {
         |        def show(x: a): String
         |    }
         |}
         |
         |namespace B {
         |    class Show[a] {
         |        def show(x: a): String
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseClass](result)
  }

  test("DuplicateTypeAlias.01") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeAlias](result)
  }

  test("DuplicateTypeAlias.02") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeAlias](result)
  }

  test("SuspiciousTypeVarName.01") {
    val input =
      s"""
         |def f(_x: List[unit]): Unit = ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.02") {
    val input =
      s"""
         |def f(_x: List[Result[Unit, bool]]): Unit = ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.03") {
    val input =
      s"""
         |def f(): List[char] = ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.04") {
    val input =
      s"""
         |def f(): Unit =
         |    let x: int = 42;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.05") {
    val input =
      s"""
         |enum A {
         |    case X(string)
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("UndefinedTypeVar.Def.01") {
    val input = "def f[a](): b = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.02") {
    val input = "def f[a](x: b): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.03") {
    val input = "def f[a, b, c](x: Option[d]): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("MismatchedTypeParamKind.Explicit.01") {
    val input = "def f[o](g: Int -> o & o): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.02") {
    val input = "def f[e](g: Int -> Int & e): e = g(123)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.03") {
    val input = "def f[a](s: #{| a}, r: {| a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.04") {
    val input = "def f[a](s: #{X(Int) | a}, r: {x: Int | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.05") {
    val input = "def f[a](r: {| a}, t: a): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.06") {
    val input = "def f[a](s: #{| a}, t: a): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.07") {
    val input = "def f[a](s: Option[{|a}]): a = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.08") {
    val input = "def f[r](a: {x: {| r}}): r = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.09") {
    val input = "def f[e](a: e): Int & not e = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.10") {
    val input = "def f[e, f](a: Map[e, f]): Int & e and f = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.11") {
    val input = "def f[a](r: {x: a | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.12") {
    val input = "def f[n](a: String ? n, b: n): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Explicit.13") {
    val input = "def f[a, b, e](g: Option[a -> b & e]): Int & not (a or b) = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.01") {
    val input = "def f(g: Int -> o & o): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.02") {
    val input = "def f(g: Int -> Int & e): e = g(123)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.03") {
    val input = "def f(s: #{| a}, r: {| a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.04") {
    val input = "def f(s: #{X(Int) | a}, r: {x: Int | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.05") {
    val input = "def f(r: {| a}, t: a): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.06") {
    val input = "def f(s: #{| a}, t: a): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.07") {
    val input = "def f(s: Option[{|a}]): a = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.08") {
    val input = "def f(a: {x: {| r}}): r = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.09") {
    val input = "def f(a: e): Int & not e = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.10") {
    val input = "def f(a: Map[e, f]): Int & e and f = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.11") {
    val input = "def f(r: {x: a | a}): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.12") {
    val input = "def f(a: String ? n, b: n): Int = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Implicit.13") {
    val input = "def f(g: Option[a -> b & e]): Int & not (a or b) = 123"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }


  test("MismatchedTypeParamKind.Enum.01") {
    val input =
      """
        |enum E[o] {
        |    case A(Int -> o & o)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.02") {
    val input =
      """
        |enum E[e] {
        |    case A((Int -> Int & e) -> e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.03") {
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, {| a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.04") {
    val input =
      """
        |enum E[a] {
        |    case A(#{X(Int) | a}, {x: Int | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.05") {
    val input =
      """
        |enum E[a] {
        |    case A({| a}, a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.06") {
    val input =
      """
        |enum E[a] {
        |    case A(#{| a}, a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.07") {
    val input =
      """enum E[a] {
        |    case A(Option[{| a}] -> a)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.08") {
    val input =
      """
        |enum E[a] {
        |    case A({x: {| r}} -> r)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.09") {
    val input =
      """
        |enum E[e] {
        |    case A(e -> Int & not e)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.10") {
    val input =
      """
        |enum E[e, f] {
        |    case A(Map[e, f] -> Int & e and f)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.11") {
    val input =
      """
        |enum E[a] {
        |    case A({x: a | a})
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.12") {
    val input =
      """
        |enum E[a, n] {
        |    case A(String ? n, n)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.Enum.13") {
    val input =
      """
        |enum E[a, b, e] {
        |    case A(Option[a -> b & e] -> Int & not (a or b))
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }


  test("MismatchedTypeParamKind.TypeAlias.01") {
    val input = "type alias T[o] = Int -> o & o"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.02") {
    val input = "type alias T[e] = (Int -> Int & e) -> e"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.03") {
    val input = "type alias T[a] = (#{| a}, {| a})"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.04") {
    val input = "type alias T[a] = (#{X(Int) | a}, {x: Int | a})"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.05") {
    val input = "type alias T[a] = ({| a}, a)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.06") {
    val input = "type alias T[a] = (#{| a}, a)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.07") {
    val input = "type alias T[a] = Option[{| a}] -> a"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.08") {
    val input = "type alias T[r] = {x: {| r}} -> r"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.09") {
    val input = "type alias T[e] = e -> Int & not e"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.10") {
    val input = "type alias T[e, f] = Map[e, f] -> Int & e and f"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.11") {
    val input = "type alias T[a] = {x: a | a}"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.12") {
    val input = "type alias T[a, n] = (String ? n, n)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("MismatchedTypeParamKind.TypeAlias.13") {
    val input = "type alias T[a, b, e] = Option[a -> b & e] -> Int & not (a or b)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }
}
