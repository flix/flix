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
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestResolver extends AnyFunSuite with TestUtils {

  test("InaccessibleDef.01") {
    val input =
      s"""
         |mod A {
         |  def f(): Int32 = 42
         |}
         |
         |mod B {
         |  def g(): Int32 = A.f()
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleDef](result)
  }

  test("InaccessibleDef.02") {
    val input =
      s"""
         |mod A {
         |  def f(): Int32 = A.B.C.g()
         |
         |  mod B.C {
         |    def g(): Int32 = A.f()
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleDef](result)
  }

  test("InaccessibleEnum.01") {
    val input =
      s"""
         |mod A {
         |  enum Color {
         |    case Blu,
         |    case Red
         |  }
         |}
         |
         |mod B {
         |  def g(): A.Color = A/Color.Red
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleEnum.02") {
    val input =
      s"""
         |mod A {
         |  def f(): A.B.C.Color = A.B.C.Color.Blu
         |
         |  mod B.C {
         |    enum Color {
         |      case Blu,
         |      case Red
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleStruct.01") {
    val input =
      s"""
         |mod A{
         |    struct S[r] {
         |        a: Int32
         |    }
         |}
         |
         |mod B {
         |    def g(): A.S = ???
         |}
         |"""
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleStruct](result)
  }

  test("InaccessibleStruct.02") {
    val input =
      s"""
         |mod A {
         |    def f(): A.B.C.Color = ???
         |
         |    mod B.C {
         |        struct Color[r] { }
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleStruct](result)
  }

  test("InaccessibleType.01") {
    val input =
      s"""
         |mod A {
         |    enum Color {
         |        case Blu,
         |        case Red
         |    }
         |}
         |
         |mod B {
         |    def g(): A.Color = ???
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleType.02") {
    val input =
      s"""
         |mod A {
         |  def f(): A.B.C.Color = ???
         |
         |  mod B.C {
         |    enum Color {
         |      case Blu,
         |      case Red
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleEnum](result)
  }

  test("InaccessibleType.03") {
    val input =
      s"""
         |mod A {
         |  def f(): A.B.C.Color = ???
         |
         |  mod B.C {
         |    struct Color[r] {
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleStruct](result)
  }

  test("InaccessibleTypeAlias.01") {
    val input =
      s"""
         |mod A {
         |  type alias Color = Int32
         |}
         |
         |mod B {
         |  def g(): A.Color = 123
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTypeAlias](result)
  }

  test("InaccessibleTypeAlias.02") {
    val input =
      s"""
         |mod A {
         |  def f(): A.B.C.Color = 123
         |
         |  mod B.C {
         |    type alias Color = Int32
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTypeAlias](result)
  }

  test("InaccessibleTrait.01") {
    val input =
      s"""
         |mod A {
         |  trait Show[a] {
         |    pub def show(x: a): String
         |  }
         |}
         |
         |mod B {
         |  def g(x: a): Int32 with A.Show[a] = ???
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTrait](result)
  }

  test("InaccessibleTrait.02") {
    val input =
      s"""
         |mod A {
         |  def f(x: a): Int32 with A.B.C.Show[a] = ???
         |
         |  mod B.C {
         |    trait Show[a] {
         |      pub def show(x: a): String
         |    }
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTrait](result)
  }

  test("InaccessibleTrait.03") {
    val input =
      """
        |mod N {
        |    trait C[a]
        |}
        |
        |mod O {
        |    instance N.C[Int32]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTrait](result)
  }

  test("InaccessibleTrait.04") {
    val input =
      """
        |mod N {
        |    trait C[a]
        |}
        |
        |mod O {
        |    trait D[a] with N.C[a]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.InaccessibleTrait](result)
  }

  test("SealedTrait.01") {
    val input =
      """
        |mod N {
        |    pub sealed trait C[a]
        |}
        |
        |mod O {
        |    instance N.C[Int32]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.SealedTrait](result)
  }

  test("SealedTrait.02") {
    val input =
      """
        |mod N {
        |    sealed trait C[a]
        |
        |    mod O {
        |        instance N.C[Int32]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.SealedTrait](result)
  }

  test("SealedTrait.03") {
    val input =
      """
        |mod N {
        |    sealed trait C[a]
        |
        |    mod O {
        |        trait D[a] with N.C[a]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.SealedTrait](result)
  }

  test("CyclicTypeAliases.01") {
    val input =
      s"""
         |type alias Foo = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  test("CyclicTypeAliases.02") {
    val input =
      s"""
         |type alias Foo = Bar
         |type alias Bar = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  test("CyclicTypeAliases.03") {
    val input =
      s"""
         |type alias Foo = Bar
         |type alias Bar = Baz
         |type alias Baz = Foo
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  test("CyclicTypeAliases.04") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  test("CyclicTypeAliases.05") {
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  ignore("CyclicTypeAliases.06") {
    val input =
      s"""
         |struct S[t, r] {
         |    field: t
         |}
         |
         |type alias Foo = S[Bar]
         |type alias Bar = S[Foo]
         |
         |def f(): Foo = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTypeAliases](result)
  }

  test("UndefinedName.01") {
    val input = "def f(): Int32 = x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.02") {
    val input =
      s"""
         |mod A {
         |  def f(x: Int32, y: Int32): Int32 = x + y + z
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.03") {
    val input =
      s"""
         |mod A {
         |    trait C[a] {
         |        pub def f(x: a): a
         |    }
         |}
         |
         |mod B {
         |    use A.f
         |    def g(): Int32 = f(1)
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedNameUnrecoverable](result)
  }

  test("UndefinedEffect.01") {
    val input =
      """
        |def f(): Unit = try () with E {
        |    def op() = ()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedEffect](result)
  }

  test("UndefinedOp.01") {
    val input =
      """
        |def f(): Unit = do E.op()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedOp](result)
  }

  test("UndefinedOp.02") {
    val input =
      """
        |eff E
        |
        |def f(): Unit = do E.op()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedOp](result)
  }

  test("UndefinedOp.03") {
    val input =
      """
        |eff E
        |
        |def f(): Unit = try () with E {
        |    def op() = ()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedOp](result)
  }

  test("UndefinedTrait.01") {
    val input =
      """
        |instance C[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTrait](result)
  }

  test("UndefinedTrait.02") {
    val input =
      """
        |def f(x: a): a with C[a] = x
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTrait](result)
  }

  test("UndefinedTrait.03") {
    val input =
      """
        |trait K[a]
        |
        |def f(x: a): a with K[a], U[a] = x
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTrait](result)
  }

  test("UndefinedTrait.04") {
    val input =
      """
        |trait K[a]
        |
        |instance K[a] with U[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTrait](result)
  }

  test("UndefinedJvmConstructor.01") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_new java.io.File(): ##java.io.File \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.02") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_new java.io.File(Int32): ##java.io.File \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.03") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_new java.lang.String(Bool): ##java.lang.String \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmConstructor.04") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_new java.lang.String(Bool, Char, String): ##java.lang.String \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmConstructor](result)
  }

  test("UndefinedJvmClass.01") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_new foo.bar.Baz(): Unit \ IO as newObject;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.02") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import foo.bar.Baz.f(): Unit \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.03") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import static foo.bar.Baz.f(): Unit \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.04") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_get_field foo.bar.Baz.f: Unit \ IO as getF;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.05") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java_set_field foo.bar.Baz.f: Unit \ IO as setF;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.06") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import static java_get_field foo.bar.Baz.f: Unit \ IO as getF;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmClass.07") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import static java_set_field foo.bar.Baz.f: Unit \ IO as setF;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmClass](result)
  }

  test("UndefinedJvmMethod.01") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.getFoo(): ##java.lang.String \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.02") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.charAt(): ##java.lang.String \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.03") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.charAt(Int32, Int32): ##java.lang.String \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.04") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.isEmpty(Bool): Bool \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.05") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import static java.lang.String.isEmpty(): Bool \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("UndefinedJvmMethod.06") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.valueOf(Bool): ##java.lang.String \ IO;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmMethod](result)
  }

  test("MismatchingReturnType.01") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.hashCode(): Unit \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.MismatchedReturnType](result)
  }

  test("MismatchingReturnType.02") {
    val input =
      raw"""
           |def foo(): Unit =
           |    import java.lang.String.subSequence(Int32, Int32): ##java.util.Iterator \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.MismatchedReturnType](result)
  }

  test("MismatchingReturnType.03") {
    val input =
      raw"""
           |type alias AliasedReturnType = ##java.util.Iterator
           |def foo(): Unit =
           |    import java.lang.String.subSequence(Int32, Int32): AliasedReturnType \ IO as _;
           |    ()
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.MismatchedReturnType](result)
  }

  test("UndefinedJvmField.01") {
    val input =
      raw"""
           |import java.lang.Math
           |
           |def foo(): Unit = Math.Foo
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.02") {
    val input =
      raw"""
           |import java.lang.Math
           |
           |def foo(): Unit = Math.Abs
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UndefinedJvmField](result)
  }

  test("UndefinedJvmField.03") {
    val input =
      raw"""
           |import java.lang.Math
           |import java.io.File
           |
           |def foo(): Unit = File.PI
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
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
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedTag.02") {
    val input =
      s"""
         |mod A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(): B = B.Qux(1 + 2)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedTag.03") {
    val input =
      s"""
         |mod A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  def f(b: B): Int32 = match b {
         |    case B.Qux => 42
         |  }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTag](result)
  }

  test("UndefinedType.01") {
    val input = "def x(): Foo = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.02") {
    val input =
      s"""mod A {
         |  def foo(bar: Baz, baz: Baz): Qux = bar
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.03") {
    val input =
      s"""
         |mod A {
         |  def f(): Int32 = Foo.Bar
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedType.04") {
    val input =
      s"""
         |mod A {
         |  def f(): Int32 = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedType.05") {
    val input =
      """
        |def f(): Unit \ E = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UndefinedType.06") {
    val input =
      """
        |def f(x: a -> b \ E): Unit = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }


  test("CyclicTraitHierarchy.01") {
    val input = "trait A[a] with A[a]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTraitHierarchy](result)
  }

  test("CyclicTraitHierarchy.02") {
    val input =
      """
        |trait A[a] with B[a]
        |trait B[a] with A[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTraitHierarchy](result)
  }

  test("CyclicTraitHierarchy.03") {
    val input =
      """
        |trait A[a] with B[a]
        |trait B[a] with C[a]
        |trait C[a] with A[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTraitHierarchy](result)
  }

  test("CyclicTraitHierarchy.04") {
    val input =
      """
        |trait A[a] with A[a], B[a]
        |trait B[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTraitHierarchy](result)
  }

  test("CyclicTraitHierarchy.05") {
    val input =
      """
        |trait A[a] with B[a]
        |trait B[a] with A[a], C[a]
        |trait C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.CyclicTraitHierarchy](result)
  }

  test("DuplicateDerivation.01") {
    val input =
      """
        |enum E with Eq, Eq
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.DuplicateDerivation](result)
  }

  test("DuplicateDerivation.02") {
    val input =
      """
        |enum E with ToString, Order, ToString
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.DuplicateDerivation](result)
  }

  test("UnderAppliedTypeAlias.01") {
    val input =
      """
        |type alias T[a] = a
        |type alias S = T
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UnderAppliedTypeAlias](result)
  }

  test("UnderAppliedTypeAlias.02") {
    val input =
      """
        |type alias T[a, b] = (a, b)
        |type alias S = T[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UnderAppliedTypeAlias](result)
  }

  test("UnderAppliedTypeAlias.03") {
    val input =
      """
        |type alias T[a] = a
        |
        |def f(x: T): Int32 = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UnderAppliedTypeAlias](result)
  }

  test("UnderAppliedTypeAlias.04") {
    val input =
      """
        |type alias T[a] = a
        |enum E[f: Type -> Type]
        |
        |def f(x: E[T]): Int32 = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.UnderAppliedTypeAlias](result)
  }

  test("UnderAppliedAssocType.01") {
    val input =
      """
        |trait C[a] {
        |    type T[a]: Type
        |}
        |
        |def f(x: C.T): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UnderAppliedAssocType](result)
  }

  test("UndefinedAssocType.01") {
    val input =
      """
        |trait C[a]
        |
        |instance C[String] {
        |    type T[String] = Int32
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedAssocType](result)
  }

  test("IllegalType.01") {
    val input =
      """
        |def isThisThingNull(x: a): Bool =
        |    import static java.util.Objects.isNull(a): Bool \ Pure;
        |    isNull(x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalType](result)
  }

  test("IllegalNonJavaType.01") {
    val input =
      """
        |def f(): Unit =
        |    new Int32 {}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalNonJavaType](result)
  }

  test("IllegalNonJavaType.02") {
    val input =
      """
        |def f(): Unit =
        |    new String {}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalNonJavaType](result)
  }

  test("IllegalNonJavaType.03") {
    val input =
      """
        |type alias T = Int32
        |
        |def f(): Unit =
        |    new T {}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalNonJavaType](result)
  }

  test("ParentNamespaceNotVisible.01") {
    val input =
      """
        |mod A {
        |    pub enum X
        |    mod B {
        |        def foo(): X = ???
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("ParentNamespaceNotVisible.02") {
    val input =
      """
        |mod A {
        |    pub type alias X = Int32
        |    mod B {
        |        def foo(): X = ???
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("ParentNamespaceNotVisible.03") {
    val input =
      """
        |mod A {
        |    pub trait X[a]
        |    mod B {
        |        enum Y
        |        instance X[Y]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTrait](result)
  }

  test("ParentNamespaceNotVisible.04") {
    val input =
      """
        |mod A {
        |    pub def x(): Int32 = ???
        |    mod B {
        |        def foo(): Int32 = x()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UseClearedInNamespace.01") {
    val input =
      """
        |use A.X
        |mod A {
        |  enum X
        |}
        |mod B {
        |  def foo(): X = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("UseClearedInNamespace.02") {
    val input =
      """
        |mod A {
        |  enum X
        |}
        |mod B {
        |  use A.X
        |  mod C {
        |     def foo(): X = ???
        |  }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("ImportClearedInNamespace.01") {
    val input =
      """
        |import java.lang.StringBuffer
        |mod A {
        |  def foo(): StringBuffer = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("ImportClearedInNamespace.02") {
    val input =
      """
        |mod A {
        |  import java.lang.StringBuffer
        |  mod B {
        |     def foo(): StringBuffer = ???
        |  }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedType](result)
  }

  test("TestParYield.01") {
    val input =
      """
        |def f(): Int32 =
        |    par (_ <- let b = 5; b) yield b
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedTypeVar.Def.01") {
    val input = "def f[a: Type](): b = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.02") {
    val input = "def f[a: Type](x: b): Int = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Def.03") {
    val input = "def f[a: Type, b: Type, c: Type](x: Option[d]): Int = 123"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.01") {
    val input = "instance C[a] with C[b]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.02") {
    val input = "instance C[(a, b)] with D[c]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Instance.03") {
    val input = "instance C[(a, b)] with D[a], D[b], D[c]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Trait.01") {
    val input =
      """
        |trait A[a]
        |trait B[a] with A[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Trait.02") {
    val input =
      """
        |trait A[a]
        |trait B[a]
        |trait C[a] with A[a], B[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("UndefinedTypeVar.Expression.01") {
    val input =
      """
        |def f(): Bool = typematch () {
        |    case _: a => true
        |    case _: _ => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedTypeVar](result)
  }

  test("IllegalSignature.01") {
    // The type variable `a` does not appear in the signature of `f`
    val input =
      """
        |trait C[a] {
        |    pub def f(): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalSignature.02") {
    val input =
      """
        |trait C[a] {
        |    pub def f(): a
        |
        |    pub def g(): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalSignature.03") {
    val input =
      """
        |trait C[a] {
        |    pub def f(x: {y = a}): {y = Bool}
        |
        |    pub def g(x: {y = Bool}): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalSignature.04") {
    val input =
      """
        |trait C[a] {
        |    pub def f(): a
        |
        |    pub def g(): Bool
        |
        |    pub def h(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalSignature.05") {
    val input =
      """
        |trait C[a] {
        |    pub def f(): Int
        |
        |    pub def g(): String
        |
        |    pub def h(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalSignature.06") {
    val input =
      """
        |trait C[a] {
        |    type T[a]: Type
        |
        |    pub def f(x: C.T[a]): String
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalSignature](result)
  }

  test("IllegalWildType.01") {
    val input =
      """
        |type alias T = _
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("IllegalWildType.02") {
    val input =
      """
        |type alias T = _ -> _
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("IllegalWildType.03") {
    val input =
      """
        |enum E {
        |    case C(_)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("IllegalWildType.04") {
    val input =
      """
        |enum E[_]
        |def foo(): String = unchecked_cast(123 as E[_])
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("IllegalWildType.05") {
    val input =
      """
        |def foo(): String \ IO = {
        |    import java.util.Arrays.deepToString(Array[_, _], Int32): String \ IO;
        |    deepToString(Array#{} @ Static)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("IllegalWildType.06") {
    val input =
      """
        |def foo(): String \ IO = {
        |    import java.util.Arrays.deepToString(Array[Int32, Static], Int32): _ \ IO;
        |    deepToString(Array#{} @ Static)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[ResolutionError.IllegalWildType](result)
  }

  test("UndefinedName.ForEachYield.01") {
    val input =
      """
        |def foo(): List[String] =
        |    foreach (x <- "1" :: "2" :: Nil; if y != "0")
        |        yield x
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.ForEachYield.02") {
    val input =
      """
        |def foo(): List[String] =
        |    foreach (x <- "1" :: "2" :: Nil)
        |        yield y
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedName.ForEachYield.03") {
    val input =
      """
        |def foo(): List[(String, Int32)] =
        |    foreach (x <- "1" :: "2" :: Nil; if y > 0; y <- 0 :: 1 :: Nil)
        |        yield (x, y)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[ResolutionError.UndefinedName](result)
  }

  test("UndefinedKind.01") {
    val input =
      """
        |trait C[a: Blah]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedKind](result)
  }

  test("UndefinedKind.02") {
    val input =
      """
        |enum E[a: Blah]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedKind](result)
  }

  test("UndefinedKind.03") {
    val input =
      """
        |def f[a: Blah](x: a): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UndefinedKind](result)
  }

  test("DuplicateAssocTypeDef.01") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |}
        |
        |instance C[String] {
        |    type T = String
        |    type T = Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.DuplicateAssocTypeDef](result)
  }

  test("MissingAssocTypeDef.01") {
    val input =
      """
        |trait C[a] {
        |    type T: Type
        |}
        |
        |instance C[String] {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.MissingAssocTypeDef](result)
  }

  test("MissingAssocTypeDef.02") {
    val input =
      """
        |mod Foo {
        |    trait Add[a] {
        |        pub type Aef: Eff
        |        pub def add(x: a, y: a): a \ Add.Aef[a]
        |    }
        |
        |    instance Foo.Add[t] {
        |        pub def add(x: t, y: t): t \ Aef[a] = ???
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.MissingAssocTypeDef](result)
  }

  test("IllegalAssocTypeApplication.01") {
    val input =
      """
        |trait C[a] {
        |    type T
        |}
        |
        |def foo(): C.T[String] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalAssocTypeApplication](result)
  }

  test("IllegalAssocTypeApplication.02") {
    val input =
      """
        |trait C[a] {
        |    type T[a]: Type
        |}
        |
        |instance C[String] {
        |    type T[String] = C.T[String]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.IllegalAssocTypeApplication](result)
  }

  test("Test.InvalidOpParamCount.Do.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit \ E = do E.op("hello", "world")
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.MismatchedOpArity](result)
  }

  test("Test.InvalidOpParamCount.Handler.01") {
    val input =
      """
        |eff E {
        |    pub def op(x: String): Unit
        |}
        |
        |def foo(): Unit = {
        |    try checked_ecast(()) with E {
        |        def op(x, y, cont) = ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.MismatchedOpArity](result)
  }

  test("ResolutionError.NonExistentStruct.01") {
    val input =
      """
        |def f(): Unit = {
        |    region rc {
        |        new NonExistentStruct{ } @ rc;
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.NonExistentStruct](result)
  }

  test("ResoutionError.MissingStructField.01") {
    val input =
      """
        |mod S {
        |    struct S[r] { }
        |    def f(s: S[r]): Unit = {
        |        s€missingField;
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.NonExistentStructField](result)
  }

  test("ResolutionError.MissingStructField.02") {
    val input =
      """
        |mod S {
        |    struct S[r] { }
        |    def f(s: S[r]): Unit = {
        |        s€missingField = 3;
        |        ()
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.NonExistentStructField](result)
  }

  test("ResolutionError.TooFewFields.01") {
    val input = """
                  |struct S[r] {
                  |    a: Int32
                  |}
                  |def f(rc: Region): S[r] = {
                  |    new S { } @ rc
                  |}
                  |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.UnprovidedStructFields](result)
  }

  test("ResolutionError.TooManyFields.01") {
    val input = """
                  |struct S[r] {
                  |    a: Int32
                  |}
                  |def f(rc: Region): S[r] = {
                  |    new S { a = 4, b = "hello" } @ rc
                  |}
                  |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ResolutionError.ExtraStructFields](result)
  }
}
