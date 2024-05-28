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
        |trait C[a]
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
        |trait C[a]
        |
        |instance C[(a, b)]
        |
        |instance C[(x, y)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.03") {
    val input =
      """
        |trait C[a]
        |
        |instance C[a -> b \ ef1]
        |
        |instance C[x -> y \ ef2]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.04") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a]
        |
        |instance C[Box[a]]
        |
        |instance C[Box[b]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstances.05") {
    val input =
      """
        | trait C[a] {
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

  test("Test.ComplexInstanceType.01") {
    val input =
      """
        |trait C[a]
        |
        |instance C[(a, Int32)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.02") {
    val input =
      """
        |trait C[a]
        |
        |instance C[Unit -> a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.03") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a]
        |
        |instance C[Box[Int32]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.05") {
    val input =
      """
        |trait C[a]
        |
        |instance C[Int32 -> b \ e]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.06") {
    val input =
      """
        |enum Box[a] {
        |    case Box(a)
        |}
        |
        |trait C[a]
        |
        |instance C[Box[a] -> b \ e]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.07") {
    val input =
      """
        |trait C[a]
        |
        |instance C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.ComplexInstanceType.08") {
    val input =
      """
        |trait C[a]
        |
        |instance C[m[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ComplexInstance](result)
  }

  test("Test.DuplicateTypeParameter.01") {
    val input =
      """
        |trait C[a]
        |
        |instance C[(a, a)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVar](result)
  }

  test("Test.DuplicateTypeParameter.02") {
    val input =
      """
        |trait C[a]
        |
        |instance C[a -> a \ ef]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVar](result)
  }

  test("Test.DuplicateTypeParameter.03") {
    val input =
      """
        |enum DoubleBox[a, b] {
        |    case DoubleBox(a, b)
        |}
        |
        |trait C[a]
        |
        |instance C[DoubleBox[a, a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.DuplicateTypeVar](result)
  }

  test("Test.MissingImplementation.01") {
    val input =
      """
        |trait C[a] {
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
        |trait C[a] {
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
        |trait C[a] {
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
        |trait C[a] {
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
        |trait C[a] {
        |    pub def f(x: b): a with D[b]
        |}
        |
        |trait D[a]
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
        |trait C[a] {
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
        |trait C[a] {
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

  test("Test.MismatchedSignatures.07") {
    val input =
      """
        |trait Foo[t] {
        |    type K: Type -> Type
        |    type E: Type

        |    pub def f(x: t): Foo.K[t][Foo.E[t]]
        |}
        |
        |enum List[_]
        |enum Set[_]
        |
        |instance Foo[Int32] {
        |    type K = List
        |    type E = String
        |    pub def f(x: Int32): List[String] = ???
        |}
        |
        |
        |instance Foo[Int64] {
        |    type K = Set
        |    type E = Char // OOPS
        |    pub def f(x: Int64): Set[String] = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[InstanceError.MismatchedSignatures](result)

  }

  test("Test.ExtraneousDefinition.01") {
    val input =
      """
        |trait C[a]
        |
        |instance C[Bool] {
        |    pub def get(): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.ExtraneousDef](result)
  }

  test("Test.OrphanInstance.01") {
    val input =
      """
        |trait C[a]
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
        |    pub trait C[a]
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
        |    trait C[a]
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
        |    trait C[a]
        |
        |    mod O {
        |        instance N.C[Int32]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.MissingSuperTraitInstance.01") {
    val input =
      """
        |trait A[a] with B[a]
        |trait B[a]
        |
        |instance A[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperTraitInstance](result)
  }

  test("Test.MissingSuperTraitInstance.02") {
    val input =
      """
        |trait A[a] with B[a], C[a]
        |trait B[a]
        |trait C[a]
        |
        |instance A[Int32]
        |instance B[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperTraitInstance](result)
  }

  test("Test.MissingSuperTraitInstance.03") {
    val input =
      """
        |trait A[a] with B[a]
        |trait B[a]
        |
        |instance A[Int32]
        |instance B[Bool]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingSuperTraitInstance](result)
  }

  test("Test.UnlawfulSignature.01") {
    val input =
      """
        |lawful trait C[a] {
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
        |lawful trait C[a] {
        |  pub def f(x: a): Bool
        |  pub def g(x: a): Bool
        |
        |  law l: forall (x: a) C.f(x)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.UnlawfulSignature](result)
  }

  test("Test.MultipleErrors.01") {
    val input =
      """
        |trait Foo[a] {
        |    pub def bar(): a
        |}
        |
        |instance Foo[String] {
        |    pub def qux(): Int32 = 2
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingImplementation](result)
    expectError[InstanceError.ExtraneousDef](result)
  }

  test("Test.IllegalOverride.01") {
    val input =
      """
        |trait C[a] {
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
        |trait C[a] {
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
        |trait C[a] {
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
    expectError[InstanceError.ComplexInstance](result)
    rejectError[InstanceError.ExtraneousDef](result)
  }

  test("Test.TypeAliasInstance.01") {
    val input =
      """
        |trait C[a]
        |type alias T = Int32
        |instance C[T]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalTypeAliasInstance](result)
  }

  test("Test.TypeAliasInstance.02") {
    val input =
      """
        |trait C[a]
        |type alias T[a] = Int32
        |instance C[T[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalTypeAliasInstance](result)
  }

  test("Test.AssocTypeInstance.01") {
    val input =
      """
        |trait C[a] {
        |    type T[a]: Type
        |}
        |
        |trait D[a]
        |
        |instance D[C.T[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.IllegalAssocTypeInstance](result)
  }

  test("Test.MissingConstraint.01") {
    val input =
      """
        |trait C[a]
        |trait D[a] with C[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingTraitConstraint](result)
  }

  test("Test.MissingConstraint.02") {
    val input =
      """
        |trait C[a]
        |trait D[a] with C[a]
        |trait E[a] with D[a]
        |
        |trait F[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)] with C[a], C[b], F[a], F[b]
        |instance E[(a, b)] with C[a], C[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[InstanceError.MissingTraitConstraint](result)
  }

  test("Test.MissingConstraint.03") {
    val input =
      """
        |trait C[a]
        |trait D[a] with C[a]
        |
        |instance C[(a, b)] with C[a], C[b]
        |instance D[(a, b)] with D[a], D[b]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    rejectError[InstanceError.MissingTraitConstraint](result)
  }
}
