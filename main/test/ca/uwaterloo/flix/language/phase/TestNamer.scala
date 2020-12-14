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
         |def foo(): Bool =
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
         |def foo(): Bool =
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

  test("DuplicateDefOrSig.01") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.02") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
         |def f(): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.04") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.05") {
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
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.06") {
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
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.07") {
    val input =
      """
        |class C[a] {
        |    def f(x: a): Int
        |    def f(x: a): Bool
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.08") {
    val input =
      """
        |class C[a] {
        |    def f(x: a): Int
        |    def f(x: a): Bool
        |    def f(x: Int): a
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.09") {
    val input =
      s"""
         |class A[a] {
         |  def f(x: a): Int
         |}
         |
         |namespace A {
         |  def f(): Int = 21
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.10") {
    val input =
      s"""
         |namespace A/B/C {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  namespace B {
         |    class C[a] {
         |      def f(x: a): Int
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateDefOrSig.11") {
    val input =
      s"""
         |namespace A/C {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  class C[a] {
         |    def f(x: a): Int
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateDefOrSig](result)
  }

  test("DuplicateUseDef.01") {
    val input =
      s"""
         |def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.02") {
    val input =
      s"""
         |use A.f;
         |use B.f;
         |
         |def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.03") {
    val input =
      s"""
         |use A.f;
         |
         |def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.04") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.{f => g, f => g};
         |    g() == g()
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDefOrSig](result)
  }


  test("DuplicateUseDef.05") {
    val input =
      s"""
         |namespace T {
         |    def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.06") {
    val input =
      s"""
         |use A.f;
         |
         |namespace T {
         |    use B.f;
         |    def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.07") {
    val input =
      s"""
         |namespace T {
         |    use A.{f => g, f => g};
         |    def foo(): Bool =
         |        g() == g()
         |}
         |
         |namespace A {
         |    pub def f(): Int = 1
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseDef.08") {
    val input =
      s"""
         |namespace T {
         |    use A.f;
         |    def foo(): Bool =
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
    expectError[NameError.DuplicateUseDefOrSig](result)
  }

  test("DuplicateUseTypeOrClass.01") {
    val input =
      s"""
         |def foo(): Bool =
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
    expectError[NameError.DuplicateUseTypeOrClass](result)
  }

  test("DuplicateUseTypeOrClass.02") {
    val input =
      s"""
         |use A.Color;
         |use B.Color;
         |
         |def foo(): Bool = true
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
    expectError[NameError.DuplicateUseTypeOrClass](result)
  }

  test("DuplicateUseTypeOrClass.03") {
    val input =
      s"""
         |namespace T {
         |    use A.Color;
         |    use B.Color;
         |    def foo(): Bool =
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
    expectError[NameError.DuplicateUseTypeOrClass](result)
  }

  test("DuplicateUseTag.01") {
    val input =
      s"""
         |def foo(): Bool =
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
         |def foo(): Bool =
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
         |def foo(): Bool =
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
         |def foo(): Bool =
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
         |    def foo(): Bool =
         |        Red == Red
         |}
         |
         |def foo(): Bool =
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
         |    def foo(): Bool =
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
         |    def foo(): Bool =
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

  test("DuplicateTypeOrClass.01") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.02") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.03") {
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
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.04") {
    val input =
      s"""
         |type alias USD = Int
         |enum USD {
         |  case A
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.05") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |enum USD {
         |  case A
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.06") {
    val input =
      s"""
         |namespace A {
         |  type alias USD = Int
         |}
         |
         |namespace A {
         |  enum USD {
         |    case B
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.07") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |enum USD {
         |  case B
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.08") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |enum  USD {
         |  case B
         |}
         |enum USD {
         |  case C
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.09") {
    val input =
      s"""
         |namespace A {
         |  enum USD {
         |    case A
         |  }
         |}
         |
         |namespace A {
         |  enum USD {
         |    case B
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.10") {
    val input =
      s"""
         |type alias USD = Int
         |class USD[a]
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.11") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |class USD[a]
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.12") {
    val input =
      s"""
         |namespace A {
         |  type alias USD = Int
         |}
         |
         |namespace A {
         |  class USD[a]
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.13") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |class USD[a]
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.14") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |enum USD {
         |  case B
         |}
         |class USD[a]
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.15") {
    val input =
      s"""
         |namespace A {
         |  enum USD {
         |    case A
         |  }
         |}
         |
         |namespace A {
         |  class USD[a]
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.16") {
    val input =
      s"""
         |class USD[a]
         |class USD[a]
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.17") {
    val input =
      s"""
         |class USD[a]
         |class USD[a]
         |class USD[a]
         """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
  }

  test("DuplicateTypeOrClass.18") {
    val input =
      s"""
         |namespace A {
         |  class USD[a]
         |}
         |
         |namespace A {
         |  class USD[a]
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.DuplicateTypeOrClass](result)
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

  test("UndefinedTypeVar.Instance.01") {
    val input = "instance C[a] with [b : C]"
    val result = compile(input, DefaultOptions)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.02") {
    val input = "instance C[(a, b)] with [c : D]"
    val result = compile(input, DefaultOptions)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.03") {
    val input = "instance C[(a, b)] with [a : D, b : D, c : D]"
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
    val input = "type alias T[a, b, e] = Option[a -> b & e] -> Int & not (a or b)"
    val result = compile(input, DefaultOptions)
    expectError[NameError.MismatchedTypeParamKinds](result)
  }

  test("IllegalSignature.01") {
    // The type variable `a` does not appear in the signature of `f`
    val input =
      """
        |class C[a] {
        |    def f(): Bool
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.02") {
    val input =
      """
        |class C[a] {
        |    def f(): a
        |
        |    def g(): Bool
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.03") {
    val input =
      """
        |class C[a] {
        |    def f(x: {y : a}): {y : Bool}
        |
        |    def g(x: {y : Bool}): Bool
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.04") {
    val input =
      """
        |class C[a] {
        |    def f(): a
        |
        |    def g(): Bool
        |
        |    def h(): a
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.05") {
    val input =
      """
        |class C[a] {
        |    def f(): Int
        |
        |    def g(): String
        |
        |    def h(): a
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[NameError.IllegalSignature](result)
  }
}
