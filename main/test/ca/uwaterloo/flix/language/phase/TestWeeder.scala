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
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestWeeder extends AnyFunSuite with TestUtils {

  test("DuplicateTag.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateTag](result)
  }

  test("DuplicateTag.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu,
        |  case Red
        |}
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateTag](result)
  }

  test("IllegalEnum.01") {
    val input =
      """
        |enum E(Int32) {
        |    case C
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  test("IllegalEnum.02") {
    val input =
      """
        |enum E(Int32) { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEnum](result)
  }

  test("IllegalJavaClass.01") {
    val input =
      """
        |import java.util.Locale$Builder
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalJavaClass](result)
  }

  test("IllegalIntrinsic.01") {
    val input =
      """
        |def f(): Unit = $NOT_A_VALID_INTRINSIC$()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedIntrinsic](result)
  }

  test("EmptyInterpolatedExpression.01") {
    val input = "def f(): String = \"${}\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("HalfInterpolationEscape.01") {
    val input = "def f(): String = \"${}\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("IllegalResume.01") {
    val input =
      """
        |def f(): Bool = resume("Hello!")
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalResume](result)
  }

  test("IllegalResume.02") {
    val input =
      """
        |def f(): Bool =
        |   try {
        |       true
        |   } catch {
        |       case _: ##java.lang.Exception => resume(true)
        |   }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalResume](result)
  }

  test("IllegalResume.03") {
    val input =
      """
        |def f(): Bool =
        |   try {
        |       resume(true)
        |   } with Fail {
        |       def fail() = false
        |   }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalResume](result)
  }

  test("MalformedUnicodeEscape.String.01") {
    // In scala, unicode escapes are preprocessed,
    // and other escapes are not processed in triple-quoted strings.
    // So we use BS in the input and replace it with a real backslash afterward.
    val input =
    """
      |def f(): String = "BSuINVALID"
      |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscapeSequence.String.02") {
    val input =
      """
        |def f(): String = "BSu000"
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Char.01") {
    val input =
      """
        |def f(): Char = 'BSuINVALID'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Char.02") {
    val input =
      """
        |def f(): Char = 'BSu000'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Interpolation.01") {
    val input =
      """
        |def f(): String = '${25}BSuINVALID'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Interpolation.02") {
    val input =
      """
        |def f(): String = '${25}BSu000'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Patten.String.01") {
    val input =
      """
        |def f(x: String): Bool = match x {
        |  case "BSuINVALID" => true
        |  case _ => false
        |}
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Patten.String.02") {
    val input =
      """
        |def f(x: String): Bool = match x {
        |  case "BSu000" => true
        |  case _ => false
        |}
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Patten.Char.01") {
    val input =
      """
        |def f(x: Char): Bool = match x {
        |  case 'BSuINVALID' => true
        |  case _ => false
        |}
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscape.Patten.Char.02") {
    val input =
      """
        |def f(x: Char): Bool = match x {
        |  case 'BSu000' => true
        |  case _ => false
        |}
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("InvalidEscapeSequence.String.01") {
    val input =
      """
        |def f(): String = "\Q"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Char.01") {
    val input =
      """
        |def f(): Char = '\Q'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Interpolation.01") {
    val input =
      """
        |def f(): String = '${25}\Q'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Patten.String.01") {
    val input =
      """
        |def f(x: String): Bool = match x {
        |  case "\Q" => true
        |  case _ => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Patten.Char.01") {
    val input =
      """
        |def f(x: Char): Bool = match x {
        |  case '\Q' => true
        |  case _ => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("HalfInterpolationEscape.02") {
    val input = s"""pub def foo(): String = "\\$$ {""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("NonSingleCharacter.Char.01") {
    val input =
      """
        |def f(): Char = 'ab'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedChar](result)
  }

  test("NonSingleCharacter.Char.02") {
    val input =
      """
        |def f(): Char = ''
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedChar](result)
  }

  test("NonSingleCharacter.Patten.Char.01") {
    val input =
      """
        |def f(x: Char): Bool = match x {
        |  case 'ab' => true
        |  case _ => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedChar](result)
  }

  test("NonLinearPattern.01") {
    val input =
      """def f(): Bool = match (21, 42) {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.02") {
    val input =
      """def f(): Bool = match (21, 42, 84) {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.03") {
    val input =
      """def f(): Bool = match (1, (2, (3, 4))) {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.04") {
    val input =
      """def f(): Bool = match { a = 1, b = 1 } {
        |  case { a = x, b = x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.05") {
    val input =
      """def f(): Bool = match { a = { b = 1 }, b = 1 } {
        |  case { a = { b = x }, b = x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.06") {
    val input =
      """def f(): Bool = match { x = 1, x = false } {
        |  case { x, x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.07") {
    val input =
      """def f(): Bool = match { x = 1, y = false } {
        |  case { x = x, y = x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.08") {
    val input =
      """def f(): Bool = match { x = 1, y = false } {
        |  case { x , y = x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonLinearPattern.09") {
    val input =
      """def f(): Bool = match { x = 1, y = false } {
        |  case { y = x, x } => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("IllegalNullPattern.01") {
    val input =
      s"""
         |def f(): Int32 = match null {
         |    case null => 123
         |    case _    => 456
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalFixedAtom.01") {
    val input =
      """def f(): Unit =
        |    let _p = #{
        |        R(x) :- A(x), not fix B(x).
        |    };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalFixedAtom](result)
  }

  test("DuplicateAnnotation.01") {
    val input =
      """@test @test
        |def foo(x: Int32): Int32 = 42
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateAnnotation](result)
  }

  test("UndefinedAnnotation.01") {
    val input =
      """@abc
        |def foo(x: Int32): Int32 = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("UndefinedAnnotation.02") {
    val input =
      """@foobarbaz
        |def foo(x: Int32): Int32 = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("IllegalModifier.01") {
    val input =
      """
        |lawful enum A
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("IllegalModifier.02") {
    val input =
      """
        |override enum A
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("IllegalModifier.03") {
    val input =
      """
        |sealed enum A
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("IllegalModifier.04") {
    val input =
      """pub instance Sub[String] {
        |    pub def sub(x: String, y: String): String = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("IllegalPrivateDeclaration.01") {
    val input =
      """
        |class C[a] {
        |    def f(): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalPrivateDeclaration.02") {
    val input =
      """
        |instance C[Int32] {
        |    def f(): Int32 = 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalEffectTypeParams.01") {
    val input =
      """
        |eff MyEffect[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalEffectTypeParams.02") {
    val input =
      """
        |eff MyEffect {
        |    def op[a](x: a): Unit
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectTypeParams](result)
  }

  test("IllegalOperationEffect.01") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ IO
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("IllegalOperationEffect.02") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ E
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEffectfulOperation](result)
  }

  test("NonUnitOperationType.01") {
    val input =
      """
        |eff E {
        |    def op(): Bool
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonUnitOperationType](result)
  }

  test("MissingFormalParamAscription.01") {
    val input =
      """
        |def f(x): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingFormalParamAscription](result)
  }

  test("IllegalFormalParamAscription.01") {
    val input =
      """
        |def f(): String =
        |    try ??? with Fail {
        |        def fail(x: String) = "hello"
        |    }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFormalParamAscription](result)
  }

  test("DuplicateFormal.01") {
    val input = "def f(x: Int32, x: Int32): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.02") {
    val input = "def f(x: Int32, y: Int32, x: Int32): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.03") {
    val input = "def f(x: Bool, x: Int32, x: Str): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.04") {
    val input = "def f(): (Int32, Int32) -> Int32 = (x, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.05") {
    val input = "def f(): (Int32, Int32, Int32) -> Int32 = (x, y, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("InconsistentTypeParameters.01") {
    val input =
      """
        |enum E[a, b: Bool] {
        |    case E1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("InconsistentTypeParameters.02") {
    val input =
      """
        |type alias T[a, b: Bool] = Int32
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("InconsistentTypeParameters.03") {
    val input =
      """
        |enum T[a, b: Bool](Int32)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("MissingTypeParamKind.01") {
    val input =
      """
        |def f[a](x: a): a = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingTypeParamKind](result)
  }

  test("MissingTypeParamKind.02") {
    val input =
      """
        |class C[a] {
        |    def f[b](x: b): a = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingTypeParamKind](result)
  }

  test("IllegalTypeConstraintParameter.01") {
    val input =
      """
        |class C[a] with D[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalTypeConstraintParameter](result)
  }

  test("IllegalTypeConstraintParameter.02") {
    val input =
      """
        |instance C[a] with D[Some[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalTypeConstraintParameter](result)
  }

  test("ReservedName.Def.01") {
    val input =
      """
        |pub def **(x: a, y: a): a = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Def.02") {
    val input =
      """
        |pub def def(x: a, y: a): a = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Sig.01") {
    val input =
      """
        |class C[a] {
        |    pub def <+>(x: a): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Sig.02") {
    val input =
      """
        |class C[a] {
        |    pub def pub(x: a): a
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Law.01") {
    val input =
      """
        |class C[a] {
        |    law **: forall (x: a) . true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Law.02") {
    val input =
      """
        |class C[a] {
        |    law law: forall (x: a) . true
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Effect.01") {
    val input =
      """
        |eff Pure
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  // TODO: IllegalFloat

  test("IllegalInt8.01") {
    val input = "def f(): Int8 = -1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt8.02") {
    val input = "def f(): Int8 = 1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt16.01") {
    val input = "def f(): Int16 = -100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt16.02") {
    val input = "def f(): Int16 = 100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt32.01") {
    val input = "def f(): Int32 = -10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt32.02") {
    val input = "def f(): Int32 = 10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt64.01") {
    val input = "def f(): Int64 = -100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalInt64.02") {
    val input = "def f(): Int64 = 100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("IllegalForFragment.01") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | foreach (if x > 0) yield 1
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.02") {
    val input =
      """
        |def f(x: Int32, ys: List[Int32]): List[Int32] =
        | foreach (if x > 0; y <- ys) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.03") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | foreach (if x > 0) println(x)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.04") {
    val input =
      """
        |def f(x: Int32, ys: List[Int32]): List[Int32] =
        | foreach (if x > 0; y <- ys) println(y)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.05") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | forM (if x > 0) yield x
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.06") {
    val input =
      """
        |def f(x: Int32, ys: List[Int32]): List[Int32] =
        | forM (if x > 0; y <- ys) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalEmptyForFragment.01") {
    val input =
      """
        |def f(): List[Int32] = foreach () 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalEmptyForFragment.02") {
    val input =
      """
        |def f(): List[Int32] = foreach () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalEmptyForFragment.03") {
    val input =
      """
        |def f(): List[Int32] = forM () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalEmptyForFragment.04") {
    val input =
      """
        |def f(): List[Int32] = forA () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalEmptyForFragment.05") {
    val input =
      """
        |def f(): Chain[String] = for () yield "a"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalUseAlias.01") {
    val input =
      """
        |mod M {
        |    def foo(): Int32 = ???
        |}
        |
        |mod N {
        |    use M.{foo => Foo}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  test("IllegalUseAlias.02") {
    val input =
      """
        |mod M {
        |    enum Enum1
        |    def foo(): Int32 = ???
        |}
        |
        |mod N {
        |    use M.{Enum1 => enum1}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  test("IllegalModuleName.01") {
    val input =
      """
        |mod mymod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.02") {
    val input =
      """
        |mod myMod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.03") {
    val input =
      """
        |mod mymod.othermod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.04") {
    val input =
      """
        |mod mymod.Othermod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.05") {
    val input =
      """
        |mod Mymod.othermod {
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.06") {
    val input =
      """
        |mod Mymod {
        |    mod othermod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.07") {
    val input =
      """
        |mod mymod {
        |    mod othermod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.08") {
    val input =
      """
        |mod mymod {
        |    mod Othermod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.09") {
    val input =
      """
        |mod mymod {
        |    mod Othermod.Thirdmod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.10") {
    val input =
      """
        |mod mymod {
        |    mod Othermod.thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.11") {
    val input =
      """
        |mod Mymod {
        |    mod othermod.thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.12") {
    val input =
      """
        |mod Mymod {
        |    mod Othermod.thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.13") {
    val input =
      """
        |mod Mymod.Othermod {
        |    mod thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.14") {
    val input =
      """
        |mod Mymod.othermod {
        |    mod thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.15") {
    val input =
      """
        |mod Mymod.othermod {
        |    mod ThirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.16") {
    val input =
      """
        |mod mymod.othermod {
        |    mod ThirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.17") {
    val input =
      """
        |mod mymod.othermod {
        |    mod thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.18") {
    val input =
      """
        |mod mymod.Othermod {
        |    mod thirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("IllegalModuleName.19") {
    val input =
      """
        |mod mymod.Othermod {
        |    mod ThirdMod {
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModuleName](result)
  }

  test("NonUnaryAssocType.01") {
    val input =
      """
        |class C[a] {
        |    type T[a, b]: Type
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonUnaryAssocType](result)
  }

  test("NonUnaryAssocType.02") {
    val input =
      """
        |instance C[Int32] {
        |    type T[Int32, b] = Int32
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonUnaryAssocType](result)
  }

  test("IllegalEqualityConstraint.01") {
    val input =
      """
        |def f(): String where Int32 ~ Int32 = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEqualityConstraint](result)
  }

  test("IllegalEqualityConstraint.02") {
    val input =
      """
        |def f(): String where Int32 ~ Elem[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEqualityConstraint](result)
  }

  test("InvalidRegularExpression.01") {
    val input =
      """
        |def f(): Regex = regex"[a-*"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("InvalidRegularExpression.02") {
    val input =
      """
        |def f(): Regex = regex"a{}"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("InvalidRegularExpression.03") {
    val input =
      """
        |def f(): Regex = regex"a{-1}"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("InvalidRegularExpression.04") {
    val input =
      """
        |def f(): Regex = regex"\\p{InvalidGroupName}*"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("IllegalRecordPattern.01") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { | r } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyRecordExtensionPattern](result)
  }

  test("IllegalRecordExtensionPattern.01") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { x | (1, 2, 3) } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordExtensionPattern](result)
  }

  test("IllegalInnerFunctionAnnotation.01") {
    val input =
      """
        |def f(): Int32 = {
        | @Test
        | def g(i) = if (i <= 0) 0 else g(i - 1);
        | g(10)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalAnnotation](result)
  }

  test("IllegalInnerFunctionAnnotation.02") {
    val input =
      """
        |def f(): Int32 = {
        | @benchmark @Tailrec
        | def g(i) = if (i <= 0) 0 else g(i - 1);
        | g(10)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalAnnotation](result)
  }

  test("DuplicateInnerFunctionAnnotation.01") {
    val input =
      """
        |def f(): Int32 = {
        | @Tailrec @Tailrec
        | def g(i) = if (i <= 0) 0 else g(i - 1);
        | g(10)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateAnnotation](result)
  }

}
