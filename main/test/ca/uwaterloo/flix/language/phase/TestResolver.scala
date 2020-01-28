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
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UnsafeFact.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(42, x).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UnsafeFact.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(42, x, 21).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UnsafeRule.01") {
    val input =
      s"""
         |rel R(x: Int)
         |
         |R(x) :- R(y).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UnsafeRule.02") {
    val input =
      s"""
         |rel R(x: Int, y: Int)
         |
         |R(x, y) :- R(x, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UnsafeRule.03") {
    val input =
      s"""
         |rel R(x: Int, y: Int, z: Int)
         |
         |R(x, y, z) :- R(x, w, z).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("AmbiguousRelationOrLattice.01") {
    val input =
      s"""
         |rel R(x: Int)
         |lat R(x: Int)
         |
         |R(42).
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.AmbiguousRelationOrLattice](result)
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

  test("InaccessibleClass.01") {
    val input =
      s"""
         |namespace A {
         |  class X[a]
         |}
         |
         |namespace B {
         |  class Y[x] <= A.X[a]
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleClass](result)
  }

  test("InaccessibleClass.02") {
    val input =
      s"""
         |namespace A {
         |  class X[a] <= A/B/C.Y[a]
         |
         |  namespace B/C {
         |    class Y[a]
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleClass](result)
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
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleDef](result)
  }

  test("InaccessibleEff.01") {
    val input =
      s"""
         |namespace A {
         |  eff f(): Int
         |}
         |
         |namespace B {
         |  def g(): Int = A.f()
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleEff](result)
  }

  test("InaccessibleEff.02") {
    val input =
      s"""
         |namespace A {
         |  def f(): Int = A/B/C.g()
         |
         |  namespace B/C {
         |    eff g(): Int
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleEff](result)
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
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleRelation.01") {
    val input =
      s"""
         |namespace A {
         |  rel R(x: Int)
         |}
         |
         |namespace B {
         |  A.R(42).
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleRelation](result)
  }

  test("InaccessibleRelation.02") {
    val input =
      s"""
         |namespace A {
         |  A/B/C.R(42).
         |
         |  namespace B/C {
         |    rel R(x: Int)
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleRelation](result)
  }

  test("InaccessibleLattice.01") {
    val input =
      s"""
         |namespace A {
         |  lat R(x: Int)
         |}
         |
         |namespace B {
         |  A.R(42).
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleLattice](result)
  }

  test("InaccessibleLattice.02") {
    val input =
      s"""
         |namespace A {
         |  A/B/C.R(42).
         |
         |  namespace B/C {
         |    lat R(x: Int)
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.InaccessibleLattice](result)
  }

  test("RecursionLimit.01") {
    val input =
      s"""
         |type alias Foo = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.04") {
    val input =
      s"""
         |type alias Foo = Option[Foo]
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("RecursionLimit.05") {
    val input =
      s"""
         |type alias Foo = Option[Bar]
         |type alias Bar = Option[Foo]
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.RecursionLimit](result)
  }

  test("UndefinedName.01") {
    val input = "def f(): Int = x"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.02") {
    val input =
      s"""
         |namespace A {
         |  def f(x: Int, y: Int): Int = x + y + z
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedClass.01") {
    val input = "class X[a] <= Y[a]"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.02") {
    val input = "class X[a] <= X[a], Y[a]"
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.03") {
    val input =
      """
        |namespace A {
        |  class Y[a]
        |}
        |
        |class X[a] <= Y[a]
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.04") {
    val input =
      """
        |namespace A {
        |  class Y[a]
        |}
        |
        |namespace B {
        |  class X[a] <= Y[a]
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.05") {
    val input =
      """
        |impl X[Bool]
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.06") {
    val input =
      """
        |impl Eq[Bool] <= X[Bool]
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.07") {
    val input =
      """
        |namespace A {
        |  class X[a]
        |}
        |
        |impl X[Bool]
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.08") {
    val input =
      """
        |namespace A {
        |  class X[a]
        |}
        |
        |namespace B {
        |  class Y[a]
        |
        |  impl X[a] <= Y[a]
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedClass.09") {
    val input =
      """
        |namespace A {
        |  class X[a]
        |}
        |
        |namespace B {
        |  class Y[a]
        |
        |  impl Y[a] <= X[a]
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedClass](result)
  }

  test("UndefinedJvmConstructor.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.io.File:__new__() as _;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.io.File:__new__(Int32) as _;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String:__new__(Bool) as _;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String:__new__(Bool, Char, String) as _;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmClass.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import foo.bar.Baz:__new__() as newObject;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import foo.bar.Baz.f();
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import foo.bar.Baz:f();
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import get foo.bar.Baz.f as getF;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.05") {
    val input =
      s"""
         |def main(): Unit =
         |    import set foo.bar.Baz.f as setF;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.06") {
    val input =
      s"""
         |def main(): Unit =
         |    import get foo.bar.Baz:f as getF;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.07") {
    val input =
      s"""
         |def main(): Unit =
         |    import set foo.bar.Baz:f as setF;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmMethod.01") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.getFoo();
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.charAt();
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.charAt(Int32, Int32);
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.isEmpty(Bool);
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.05") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String:isEmpty();
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.06") {
    val input =
      s"""
         |def main(): Unit =
         |    import java.lang.String.valueOf(Bool);
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
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
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.02") {
    val input =
      s"""
         |def main(): Unit =
         |    import set java.lang.Character.foo as setFoo;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.03") {
    val input =
      s"""
         |def main(): Unit =
         |    import get java.lang.Character:foo as getFoo;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.04") {
    val input =
      s"""
         |def main(): Unit =
         |    import set java.lang.Character:foo as setFoo;
         |    ()
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UndefinedJvmField](result)
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

  test("UnhandledEffect.01") {
    val input =
      s"""
         |eff f(): Int
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UnhandledEffect](result)
  }

  test("UnhandledEffect.02") {
    val input =
      s"""
         |namespace A {
         |  eff f(): Int
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    expectError[ResolutionError.UnhandledEffect](result)
  }


}
