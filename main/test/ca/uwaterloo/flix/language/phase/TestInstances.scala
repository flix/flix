/*
 * Copyright 2020 Matthew Lutze
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestInstances extends AnyFunSuite with TestUtils {

  test("Test.OverlappingInstance.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int32]
        |
        |instance C[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }


  test("Test.OverlappingInstance.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |
        |instance C[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.03") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, b)]
        |
        |instance C[(x, y)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.04") {
    val input =
      """
        |class C[a]
        |
        |instance C[a -> b \ ef1]
        |
        |instance C[x -> y \ ef2]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.05") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a]
        |
        |instance C[Box[a]]
        |
        |instance C[Box[b]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstances.06") {
    val input =
      """
        | class C[a] {
        |  pub def f(x: a, y: a): Bool
        |}
        |
        |instance C[String] {
        |  pub def f(_x: String, _y: String): Bool = true
        |}
        |
        |instance C[String] {
        |  pub def f(_x: String, _y: String): Bool = true
        |}
        |
        |def g(x: String): Bool = C.f(x, x)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstances.Bool.01") {
    val input =
      """
        | class C[a]
        |
        |enum E[_: Bool]
        |
        |instance C[E[true]]
        |instance C[E[false]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstances.Bool.02") {
    val input =
      """
        | class C[a]
        |
        |enum E[_: Bool, _: Type]
        |
        |instance C[E[true, a]]
        |instance C[E[false, a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.ComplexInstanceType.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, Int32)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[Unit -> a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.03") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a]
        |
        |instance C[Box[Int32]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.05") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int32 -> b \ e]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.06") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |class C[a]
        |
        |instance C[Box[a] -> b \ e]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.07") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.DuplicateTypeParameter.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, a)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVariableOccurrence](result)
  }

  test("Test.DuplicateTypeParameter.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a -> a \ ef]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVariableOccurrence](result)
  }

  test("Test.DuplicateTypeParameter.03") {
    val input =
      """
        |enum DoubleBox[a, b] {
        |    case DoubleBox(a, b)
        |}
        |
        |class C[a]
        |
        |instance C[DoubleBox[a, a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVariableOccurrence](result)
  }

  test("Test.MissingImplementation.01") {
    val input =
      """
        |class C[a] {
        |    pub def get(): a
        |}
        |
        |instance C[Bool] {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingImplementation](result)
  }

  test("Test.MismatchedSignatures.01") {
    val input =
      """
        |class C[a] {
        |    pub def get(): a
        |}
        |
        |instance C[Bool] {
        |    pub def get(_i: Int32): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.02") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): Bool
        |}
        |
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |instance C[Box[a]] {
        |    pub def f(_x: Box[a]): Bool with C[a] = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchSignatures.03") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): String
        |}
        |
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |instance C[Int32] {
        |    pub def f(x: Int32): String = ""
        |}
        |
        |instance C[Box[a]] {
        |    def f[a: C](x: Box[a]): String = match x {
        |        case Box(y) => f(y)
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[CompilationMessage](result) // TODO should be MismatchedSignature
  }

  test("Test.MismatchedSignatures.04") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: b): a with D[b]
        |}
        |
        |class D[a]
        |
        |instance C[Bool] {
        |    pub def f(x: b): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.05") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a, y: Int32): Int32
        |}
        |
        |instance C[Bool] {
        |    pub def f(x: Bool, y: Int32): Int32 \ IO = checked_ecast(123)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.06") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a, y: Int32): Int32 \ e
        |}
        |
        |instance C[Bool] {
        |    pub def f(x: Bool, y: Int32): Int32 \ IO = checked_ecast(123)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.ExtraneousDefinition.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Bool] {
        |    pub def get(): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ExtraneousDefinition](result)
  }

  test("Test.OrphanInstance.01") {
    val input =
      """
        |class C[a]
        |
        |mod C {
        |    instance C[Int32]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.02") {
    val input =
      """
        |mod N {
        |    pub class C[a]
        |}
        |
        |instance N.C[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.03") {
    val input =
      """
        |mod N {
        |    class C[a]
        |
        |    mod C {
        |        instance N.C[Int32]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.04") {
    val input =
      """
        |mod N {
        |    class C[a]
        |
        |    mod O {
        |        instance N.C[Int32]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.MissingSuperClassInstance.01") {
    val input =
      """
        |class A[a] with B[a]
        |class B[a]
        |
        |instance A[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.MissingSuperClassInstance.02") {
    val input =
      """
        |class A[a] with B[a], C[a]
        |class B[a]
        |class C[a]
        |
        |instance A[Int32]
        |instance B[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.MissingSuperClassInstance.03") {
    val input =
      """
        |class A[a] with B[a]
        |class B[a]
        |
        |instance A[Int32]
        |instance B[Bool]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.UnlawfulSignature.01") {
    val input =
      """
        |lawful class C[a] {
        |    pub def f(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.UnlawfulSignature](result)
  }

  test("Test.UnlawfulSignature.02") {
    val input =
      """
        |instance C[Int32] {
        |    pub def f(x: Int32): Bool = true
        |    pub def g(x: Int32): Bool = true
        |}
        |
        |lawful class C[a] {
        |  pub def f(x: a): Bool
        |  pub def g(x: a): Bool
        |
        |  law l: forall (x: a) . C.f(x)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.UnlawfulSignature](result)
  }

  test("Test.MultipleErrors.01") {
    val input =
      """
        |class Foo[a] {
        |    pub def bar(): a
        |}
        |
        |instance Foo[String] {
        |    pub def qux(): Int32 = 2
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingImplementation](result)
    expectError[InstanceError.ExtraneousDefinition](result)
  }

  test("Test.IllegalOverride.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): Bool
        |}
        |
        |instance C[Int32] {
        |  override pub def f(x: Int32): Bool = true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalOverride](result)
  }

  test("Test.UnmarkedOverride.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): Bool = true
        |}
        |
        |instance C[Int32] {
        |  pub def f(x: Int32): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.UnmarkedOverride](result)
  }

  test("Test.ComplexErrorSuppressesOtherErrors.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): Bool
        |}
        |
        |instance C[Box[String]] {
        |  pub def g(_x: Box[String]): Bool = false
        |}
        |
        |enum Box[a] {
        |    case Box(a)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstanceType](result)
    rejectError[InstanceError.ExtraneousDefinition](result)
  }

  test("Test.TypeAliasInstance.01") {
    val input =
      """
        |class C[a]
        |type alias T = Int32
        |instance C[T]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalTypeAliasInstance](result)
  }

  test("Test.TypeAliasInstance.02") {
    val input =
      """
        |class C[a]
        |type alias T[a] = Int32
        |instance C[T[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalTypeAliasInstance](result)
  }

  test("Test.AssocTypeInstance.01") {
    val input =
      """
        |class C[a] {
        |    type T[a]: Type
        |}
        |
        |class D[a]
        |
        |instance D[C.T[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalAssocTypeInstance](result)
  }

  test("Test.MissingConstraint.01") {
    val input =
      """
        |class C[a]
        |class D[a] with C[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingConstraint](result)
  }

  test("Test.MissingConstraint.02") {
    val input =
      """
        |class C[a]
        |class D[a] with C[a]
        |class E[a] with D[a]
        |
        |class F[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)] with C[a], C[b], F[a], F[b]
        |instance E[(a, b)] with C[a], C[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingConstraint](result)
  }

  test("Test.MissingConstraint.03") {
    val input =
      """
        |class C[a]
        |class D[a] with C[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)] with D[a], D[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    rejectError[InstanceError.MissingConstraint](result)
  }
}
