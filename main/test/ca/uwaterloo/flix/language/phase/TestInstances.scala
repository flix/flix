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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.errors.InstanceError
import ca.uwaterloo.flix.util.{Options, Validation}
import org.scalatest.FunSuite

class TestInstances extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.TestWithLibMin

  test("Test.OverlappingInstance.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }


  test("Test.OverlappingInstance.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |
        |instance C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.OverlappingInstance.04") {
    val input =
      """
        |class C[a]
        |
        |instance C[a -> b & e]
        |
        |instance C[x -> y & e]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OverlappingInstances](result)
  }

  test("Test.ComplexInstanceType.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, Int)]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[() -> a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
        |instance C[Box[Int]]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.04") {
    val input =
      """
        |class C[a]
        |
        |instance C[a -> b]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.05") {
    val input =
      """
        |class C[a]
        |
        |instance C[Int -> b & e]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
        |instance C[Box[a] -> b & e]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.ComplexInstanceType.07") {
    val input =
      """
        |class C[a]
        |
        |instance C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
  }

  test("Test.DuplicateTypeParameter.01") {
    val input =
      """
        |class C[a]
        |
        |instance C[(a, a)]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.DuplicateTypeVariableOccurrence](result)
  }

  test("Test.DuplicateTypeParameter.02") {
    val input =
      """
        |class C[a]
        |
        |instance C[a -> a & e]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
        |    pub def get(_i: Int): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
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
        |instance C[Int] {
        |    pub def f(x: Int): String = ""
        |}
        |
        |instance C[Box[a]] {
        |    def f[a: C](x: Box[a]): String = match x {
        |        case Box(y) => f(y)
        |    }
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[CompilationError](result) // TODO should be MismatchedSignature
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
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.05") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a, y: Int): Int
        |}
        |
        |instance C[Bool] {
        |    pub def f(x: Bool, y: Int): Int & Impure = 123 as & Impure
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MismatchedSignatures](result)
  }

  test("Test.MismatchedSignatures.06") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a, y: Int): Int & e
        |}
        |
        |instance C[Bool] {
        |    pub def f(x: Bool, y: Int): Int & Impure = 123 as & Impure
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ExtraneousDefinition](result)
  }

  test("Test.OrphanInstance.01") {
    val input =
      """
        |class C[a]
        |
        |namespace C {
        |    instance C[Int]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.02") {
    val input =
      """
        |namespace N {
        |    pub class C[a]
        |}
        |
        |instance N.C[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.03") {
    val input =
      """
        |namespace N {
        |    class C[a]
        |
        |    namespace C {
        |        instance N.C[Int]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.OrphanInstance.04") {
    val input =
      """
        |namespace N {
        |    class C[a]
        |
        |    namespace O {
        |        instance N.C[Int]
        |    }
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.OrphanInstance](result)
  }

  test("Test.MissingSuperClassInstance.01") {
    val input =
      """
        |class A[a] with B[a]
        |class B[a]
        |
        |instance A[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.MissingSuperClassInstance.02") {
    val input =
      """
        |class A[a] with B[a], C[a]
        |class B[a]
        |class C[a]
        |
        |instance A[Int]
        |instance B[Int]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.MissingSuperClassInstance.03") {
    val input =
      """
        |class A[a] with B[a]
        |class B[a]
        |
        |instance A[Int]
        |instance B[Bool]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.MissingSuperClassInstance](result)
  }

  test("Test.LawlessSuperClass.01") {
    val input =
      """
        |lawless class A[a]
        |class B[a] with A[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.LawlessSuperClass](result)
  }

  test("Test.LawlessSuperClass.02") {
    val input =
      """
        |lawless class A[a]
        |class B[a]
        |class C[a] with A[a], B[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.LawlessSuperClass](result)
  }

  test("Test.UnlawfulSignature.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(): a
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.UnlawfulSignature](result)
  }

  test("Test.UnlawfulSignature.02") {
    val input =
      """
        |instance C[Int] {
        |  pub def f(x: Int): Bool = true
        |  pub def g(x: Int): Bool = true
        |}
        |
        |class C[a] {
        |  pub def f(x: a): Bool
        |  pub def g(x: a): Bool
        |
        |  law l: forall (x: a) . C.f(x)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.UnlawfulSignature](result)
  }

  test("Test.MultipleErrors.01") {
    val input =
      """
        |lawless class Foo[a] {
        |    pub def bar(): a
        |}
        |
        |instance Foo[String] {
        |    pub def qux(): Int32 = 2
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
        |instance C[Int] {
        |  override pub def f(x: Int): Bool = true
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.IllegalOverride](result)
  }

  test("Test.UnmarkedOverride.01") {
    val input =
      """
        |class C[a] {
        |  pub def f(x: a): Bool = true
        |}
        |
        |instance C[Int] {
        |  pub def f(x: Int): Bool = false
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
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
    val result = compile(input, DefaultOptions)
    expectError[InstanceError.ComplexInstanceType](result)
    rejectError[InstanceError.ExtraneousDefinition](result)
  }
}
