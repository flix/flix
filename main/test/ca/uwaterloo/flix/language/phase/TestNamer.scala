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

  test("AmbiguousVarOrUse.01") {
    val input =
      s"""
         |def foo(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    f(123)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.AmbiguousVarOrUse](result)
  }

  test("DuplicateLowerName.01") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.02") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(): Int = 21
         |def f(): Int = 11
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.03") {
    val input =
      s"""
         |def f(x: Int): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Int): Int = 11
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.04") {
    val input =
      s"""
         |def f(): Int = 42
         |def f(x: Int): Int = 21
         |def f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.05") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.06") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.07") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): Int
        |    pub def f(x: a): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.08") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): Int
        |    pub def f(x: a): Bool
        |    pub def f(x: Int): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.09") {
    val input =
      s"""
         |class A[a] {
         |  pub def f(x: a): Int
         |}
         |
         |namespace A {
         |  pub def f(): Int = 21
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.10") {
    val input =
      s"""
         |namespace A/B/C {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  namespace B {
         |    class C[a] {
         |      pub def f(x: a): Int
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.11") {
    val input =
      s"""
         |namespace A/C {
         |  def f(): Int = 42
         |}
         |
         |namespace A {
         |  class C[a] {
         |    pub def f(x: a): Int
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.12") {
    val input =
      """
        |namespace N {
        |    def f(): Int32 = 123
        |}
        |
        |eff N {
        |    pub def f(): Unit
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
  }

  test("DuplicateLowerName.13") {
    val input =
      """
        |eff N {
        |    pub def f(): Unit
        |    pub def f(): Unit
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateLowerName](result)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUseTag](result)
  }

  test("DuplicateUpperName.01") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.02") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |type alias USD = Int
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.03") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.04") {
    val input =
      s"""
         |type alias USD = Int
         |enum USD {
         |  case A
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.05") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |enum USD {
         |  case A
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.06") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.07") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |enum USD {
         |  case B
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.08") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.09") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.10") {
    val input =
      s"""
         |type alias USD = Int
         |class USD[a]
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.11") {
    val input =
      s"""
         |type alias USD = Int
         |type alias USD = Int
         |class USD[a]
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.12") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.13") {
    val input =
      s"""
         |enum USD {
         |  case A
         |}
         |class USD[a]
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.14") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.15") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.16") {
    val input =
      s"""
         |class USD[a]
         |class USD[a]
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.17") {
    val input =
      s"""
         |class USD[a]
         |class USD[a]
         |class USD[a]
         """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("DuplicateUpperName.18") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.DuplicateUpperName](result)
  }

  test("SuspiciousTypeVarName.01") {
    val input =
      s"""
         |def f(_x: List[unit]): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.02") {
    val input =
      s"""
         |def f(_x: List[Result[Unit, bool]]): Unit = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.03") {
    val input =
      s"""
         |def f(): List[char] = ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("SuspiciousTypeVarName.04") {
    val input =
      s"""
         |enum A {
         |    case X(string)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.SuspiciousTypeVarName](result)
  }

  test("UndefinedTypeVar.Def.01") {
    val input = "def f[a: Type](): b = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.02") {
    val input = "def f[a: Type](x: b): Int = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.03") {
    val input = "def f[a: Type, b: Type, c: Type](x: Option[d]): Int = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.01") {
    val input = "instance C[a] with C[b]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.02") {
    val input = "instance C[(a, b)] with D[c]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.03") {
    val input = "instance C[(a, b)] with D[a], D[b], D[c]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Class.01") {
    val input =
      """
        |class A[a]
        |class B[a] with A[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Class.02") {
    val input =
      """
        |class A[a]
        |class B[a]
        |class C[a] with A[a], B[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.UndefinedTypeVar](result)
  }

  test("IllegalSignature.01") {
    // The type variable `a` does not appear in the signature of `f`
    val input =
      """
        |class C[a] {
        |    pub def f(): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.02") {
    val input =
      """
        |class C[a] {
        |    pub def f(): a
        |
        |    pub def g(): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.03") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: {y :: a}): {y :: Bool}
        |
        |    pub def g(x: {y :: Bool}): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.04") {
    val input =
      """
        |class C[a] {
        |    pub def f(): a
        |
        |    pub def g(): Bool
        |
        |    pub def h(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.IllegalSignature](result)
  }

  test("IllegalSignature.05") {
    val input =
      """
        |class C[a] {
        |    pub def f(): Int
        |
        |    pub def g(): String
        |
        |    pub def h(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[NameError.IllegalSignature](result)
  }
}
