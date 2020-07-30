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
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

class TestResolver extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.AmbiguousTag](result)
  }

  test("InaccessibleDef.01") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = 42
         |}
         |
         |namespace B {
         |  def g(): Int = A.f()
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleDef](result)
  }

  test("InaccessibleDef.02") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = A/B/C.g()
         |
         |  namespace B/C {
         |    def g(): Int = A.f()
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleDef](result)
  }

  test("InaccessibleEnum.01") {
    val input =
      s"""
         |namespace A {
         |  enum Color {
         |    case Blu,
         |    case Red
         |  }
         |}
         |
         |namespace B {
         |  def g(): A.Color = A/Color.Red
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleEnum.02") {
    val input =
      s"""
         |namespace A {
         |  def f(): A/B/C.Color = A/B/C/Color.Blu
         |
         |  namespace B/C {
         |    enum Color {
         |      case Blu,
         |      case Red
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleType.01") {
    val input =
      s"""
         |namespace A {
         |  enum Color {
         |    case Blu,
         |    case Red
         |  }
         |}
         |
         |namespace B {
         |  def g(): A.Color = ???
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleType.02") {
    val input =
      s"""
         |namespace A {
         |  def f(): A/B/C.Color = ???
         |
         |  namespace B/C {
         |    enum Color {
         |      case Blu,
         |      case Red
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("RecursionLimit.01") {
    val input =
      s"""
         |type alias Foo = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.02") {
    val input =
      s"""
         |type alias Foo = Bar
         |type alias Bar = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.03") {
    val input =
      s"""
         |type alias Foo = Bar
         |type alias Bar = Baz
         |type alias Baz = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.04") {
    val input =
      s"""
         |enum Option[t] {
         |    case None,
         |    case Some(t)
         |}
         |
         |type alias Foo = Option[Foo]
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.05") {
    val input =
      s"""
         |enum Option[t] {
         |    case None,
         |    case Some(t)
         |}
         |
         |type alias Foo = Option[Bar]
         |type alias Bar = Option[Foo]
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("UndefinedName.01") {
    val input = "def f(): Int = x"
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.02") {
    val input =
      s"""
         |namespace A {
         |  def f(x: Int, y: Int): Int = x + y + z
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.03") {
    val input =
      s"""
         |def main(): #{ R } = #{}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedJvmConstructor.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import new java.io.File() as _;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import new java.io.File(Int32) as _;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import new java.lang.String(Bool) as _;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import new java.lang.String(Bool, Char, String) as _;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmClass.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import new foo.bar.Baz() as newObject;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import foo.bar.Baz.f();
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import foo.bar.Baz:f();
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import get foo.bar.Baz.f as getF;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.05") {
    val input =
      s"""
         |def main(): Unit =
         |    import set foo.bar.Baz.f as setF;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.06") {
    val input =
      s"""
         |def main(): Unit =
         |    import get foo.bar.Baz:f as getF;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.07") {
    val input =
      s"""
         |def main(): Unit =
         |    import set foo.bar.Baz:f as setF;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmMethod.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.getFoo();
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.charAt();
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.charAt(Int32, Int32);
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.isEmpty(Bool);
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.05") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String:isEmpty();
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.06") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.valueOf(Bool);
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmField.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import get java.lang.Character.foo as getFoo;
         |    ()
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import set java.lang.Character.foo as setFoo;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import get java.lang.Character:foo as getFoo;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import set java.lang.Character:foo as setFoo;
         |    ()
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedTag.01") {
    val input =
      s"""
         |enum A {
         |  case Foo
         |}
         |
         |def f(): A = A.Qux
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.02") {
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
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedTag.03") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(b: B): Int = match b {
         |    case B.Qux => 42
         |  }
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedType.01") {
    val input = "def x(): Foo = 42"
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.02") {
    val input =
      s"""namespace A {
         |  def foo(bar: Baz, baz: Baz): Qux = bar
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.03") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = Foo.Bar
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.04") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("ImproperType.01") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.IllegalUninhabitedType](result)
  }

  test("ImproperType.02") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |enum E {
        |  case A(P[Int])
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.IllegalUninhabitedType](result)
  }

  test("IllegalTypeApplication.03") {
    val input =
      """
        |enum P[a, b] {
        |  case C(a, b)
        |}
        |
        |def f(p: P[Int, String, String]): Int = 123
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[ResolutionError.IllegalTypeApplication](result)
  }
}
