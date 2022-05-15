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
import org.scalatest.FunSuite

class TestWeeder extends FunSuite with TestUtils {

  test("DuplicateAnnotation.01") {
    val input =
      """@test @test
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateAnnotation](result)
  }

  test("DuplicateFormal.01") {
    val input = "def f(x: Int, x: Int): Int = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.02") {
    val input = "def f(x: Int, y: Int, x: Int): Int = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.03") {
    val input = "def f(x: Bool, x: Int, x: Str): Int = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.04") {
    val input = "def f(): (Int, Int) -> Int = (x, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormal.05") {
    val input = "def f(): (Int, Int, Int) -> Int = (x, y, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

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

  test("IllegalFieldName.01") {
    val input = "def f(): { length :: Int } = { length = 123 }"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.02") {
    val input = "def f(): { length :: Int } = { +length = 123 | {} }"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.03") {
    val input = "def f(): { length :: Int } = { -length | {} }"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFieldName.04") {
    val input = "def f(): { length :: Int } = { length = 123 | {} }"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFieldName](result)
  }

  test("IllegalFixedAtom.01") {
    val input =
      """def f(): Unit =
        |    let _p = #{
        |        R(x) :- A(x), not fix B(x).
        |    };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFixedAtom](result)
  }

  test("IllegalInt8.01") {
    val input = "def f(): Int8 = -1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt8.02") {
    val input = "def f(): Int8 = 1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.01") {
    val input = "def f(): Int16 = -100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt16.02") {
    val input = "def f(): Int16 = 100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.01") {
    val input = "def f(): Int32 = -10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt32.02") {
    val input = "def f(): Int32 = 10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.01") {
    val input = "def f(): Int64 = -100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalInt64.02") {
    val input = "def f(): Int64 = 100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalInt](result)
  }

  test("IllegalNullPattern.01") {
    val input =
      s"""
         |def f(): Int = match null {
         |    case null => 123
         |    case _    => 456
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalJvmFieldOrMethodName.01") {
    val input =
      s"""
         |def f(): Unit =
         |    import foo(): Unit & Impure as bar;
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalJvmFieldOrMethodName](result)
  }

  test("MismatchedArity.01") {
    val input =
      """def f(): Bool =
        |    choose 123 {
        |        case (Present(x), Present(y)) => x == y
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedArity](result)
  }

  test("MismatchedArity.02") {
    val input =
      """def f(): Bool =
        |    choose (123, 456) {
        |        case Present(x) => x == x
        |    }
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedArity](result)
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

  test("UndefinedAnnotation.01") {
    val input =
      """@abc
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("UndefinedAnnotation.02") {
    val input =
      """@foobarbaz
        |def foo(x: Int): Int = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedAnnotation](result)
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
        |instance C[Int] {
        |    def f(): Int = 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalTypeConstraintParameter.01") {
    val input =
      """
        |class C[a] with D[Int]
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

  test("InconsistentTypeParameters.01") {
    val input =
      """
        |enum E[a, b: Bool] {
        |    case E1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InconsistentTypeParameters](result)
  }

  test("InconsistentTypeParameters.02") {
    val input =
      """
        |type alias T[a, b: Bool] = Int
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InconsistentTypeParameters](result)
  }

  test("InconsistentTypeParameters.03") {
    val input =
      """
        |opaque type T[a, b: Bool] = Int
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InconsistentTypeParameters](result)
  }

  test("UnkindedTypeParameters.01") {
    val input =
      """
        |def f[a](x: a): a = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnkindedTypeParameters](result)
  }

  test("UnkindedTypeParameters.02") {
    val input =
      """
        |class C[a] {
        |    def f[b](x: b): a = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnkindedTypeParameters](result)
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
    expectError[WeederError.InvalidEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Char.01") {
    val input =
      """
        |def f(): Char = '\Q'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InvalidEscapeSequence](result)
  }

  test("InvalidEscapeSequence.Interpolation.01") {
    val input =
      """
        |def f(): String = '${25}\Q'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InvalidEscapeSequence](result)
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
    expectError[WeederError.InvalidEscapeSequence](result)
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
    expectError[WeederError.InvalidEscapeSequence](result)
  }

  test("NonSingleCharacter.Char.01") {
    val input =
      """
        |def f(): Char = 'ab'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonSingleCharacter](result)
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
    expectError[WeederError.NonSingleCharacter](result)
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

  test("HalfInterpolationEscape.02") {
    val input = s"""pub def foo(): String = "\\$$ {""""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InvalidEscapeSequence](result)
  }

  test("IllegalModifier.01") {
    val input = "pub instance I[a]"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("ReservedName.Def.01") {
    val input =
      """
        |pub def |||(x: a, y: a): a = ???
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
        |    law ^^^: forall (x: a) . true
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

  test("ReservedName.Effect.02") {
    val input =
      """
        |eff Read
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
  }

  test("ReservedName.Effect.03") {
    val input =
      """
        |eff Write
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.ReservedName](result)
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

  test("IllegalOperationEffect.01") {
    val input =
      """
        |eff E {
        |    def op(): Unit & Impure
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalOperationEffect](result)
  }

  test("IllegalOperationEffect.02") {
    val input =
      """
        |eff E {
        |    def op(): Unit \ E
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalOperationEffect](result)
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
}
