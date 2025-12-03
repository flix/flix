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
import ca.uwaterloo.flix.language.errors.{ParseError, WeederError}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestWeeder extends AnyFunSuite with TestUtils {

  test("DuplicateAnnotation.01") {
    val input =
      """@test @test
        |def foo(x: Int32): Int32 = 42
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateAnnotation](result)
  }

  test("DuplicateAnnotation.02") {
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

  test("DuplicateFormalParam.01") {
    val input = "def f(x: Int32, x: Int32): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormalParam.02") {
    val input = "def f(x: Int32, y: Int32, x: Int32): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormalParam.03") {
    val input = "def f(x: Bool, x: Int32, x: Str): Int32 = 42"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormalParam.04") {
    val input = "def f(): (Int32, Int32) -> Int32 = (x, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateFormalParam.05") {
    val input = "def f(): (Int32, Int32, Int32) -> Int32 = (x, y, x) -> x"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateFormalParam](result)
  }

  test("DuplicateStructField.01") {
    val input =
      """struct Person[r] {
         name: String,
         name: String
      }
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateStructField](result)
  }

  test("DuplicateStructField.02") {
    val input =
      """struct Person[r] {
         name: String,
         age: Int32,
         name: String
      }
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateStructField](result)
  }

  test("DuplicateStructField.03") {
    val input =
      """struct Person[r] {
         name: String,
         age: Int32,
         name: String,
         age: Int32
      }
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateStructField](result)
  }

  test("DuplicateStructField.04") {
    val input =
      """struct Person[r] {
         name: String,
         age: Int32,
         age: Int32,
         height: Int32
      }
    """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.DuplicateStructField](result)
  }

  test("EmptyForFragment.01") {
    val input =
      """
        |def f(): List[Int32] = foreach () 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("EmptyForFragment.02") {
    val input =
      """
        |def f(): List[Int32] = foreach () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("EmptyForFragment.03") {
    val input =
      """
        |def f(): List[Int32] = forM () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("EmptyForFragment.04") {
    val input =
      """
        |def f(): List[Int32] = forA () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("EmptyInterpolatedExpression.01") {
    val input = "def f(): String = \"${}\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyInterpolatedExpression.02") {
    val input = "def f(): String = \"abc${}\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyInterpolatedExpression.03") {
    val input = "def f(): String = \"${}xyz\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyInterpolatedExpression.04") {
    val input = "def f(): String = \"abc${}xyz\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyInterpolatedExpression.05") {
    val input = "def f(): String = \"${}${}\""
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyInterpolatedExpression.06") {
    val input = """def f(): String = "${"${}"}" """
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyInterpolatedExpression](result)
  }

  test("EmptyRecordExtensionPattern.01") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { | r } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyRecordExtensionPattern](result)
  }

  test("EmptyRecordExtensionPattern.02") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { | _ } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyRecordExtensionPattern](result)
  }

  test("EmptyRecordExtensionPattern.03") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { | { | _ } } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyRecordExtensionPattern](result)
  }

  test("IllegalAnnotation.01") {
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

  test("IllegalAnnotation.02") {
    val input =
      """
        |def f(): Int32 = {
        |  @test @Tailrec
        |  def g(i) = if (i <= 0) 0 else g(i - 1);
        |  g(10)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalAnnotation](result)
  }

  test("IllegalAnnotation.03") {
    val input =
      """
        |def f(): Int32 = {
        | @Skip @Tailrec
        | def g(i) = if (i <= 0) 0 else g(i - 1);
        | g(10)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalAnnotation](result)
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
        |enum Foo()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
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

  test("IllegalEqualityConstraint.03") {
    val input =
      """
        |def f(): String where Int32 ~ Int64 = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEqualityConstraint](result)
  }

  test("IllegalEscapeSequence.Char.01") {
    val input =
      """
        |def f(): Char = '\Q'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("IllegalEscapeSequence.Interpolation.01") {
    val input =
      """
        |def f(): String = "${25}\Q"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("IllegalEscapeSequence.Pattern.Char.01") {
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

  test("IllegalEscapeSequence.Pattern.String.01") {
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

  test("IllegalEscapeSequence.String.01") {
    val input =
      """
        |def f(): String = "\Q"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalEscapeSequence](result)
  }

  test("IllegalExtPattern.01") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case 1 => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.02") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A.B(2) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.03") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A((1, 2)) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.04") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A(B(1)) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.05") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A(1) => 1
        |    case A(1) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.06") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A(1) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.07") {
    val input =
      """
        |def f(): Int32 = ematch xvar A(1) {
        |    case A(1) => 1
        |    case A(1) => 1
        |    case A(1) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.08") {
    val input =
      """
        |def f(): Bool = ematch xvar ABC() {
        |    case ABC(()) => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
  }

  test("IllegalExtPattern.09") {
    val input =
      """
        |def f(): Bool = ematch xvar ABC((), ()) {
        |    case ABC((), ()) => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalExtPattern](result)
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

  test("IllegalForAFragment.01") {
    val input =
      """
        |def f(): List[Int32] =
        | forA (x = 11) yield x
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForAFragment](result)
  }

  test("IllegalForAFragment.02") {
    val input =
      """
        |def f(ys: List[Int32]): List[Int32] =
        | forA (x = 2; y <- ys) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForAFragment](result)
  }

  test("IllegalForAFragment.03") {
    val input =
      """
        |def f(ys: List[Int32]): List[Int32] =
        | forA (y <- ys; if y > 2) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForAFragment](result)
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

  test("IllegalForFragment.07") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | foreach (x = 10) yield 1
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.08") {
    val input =
      """
        |def f(): List[Int32] =
        | foreach (y = 10) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.09") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | foreach (a = 2; if x > 0) println(x)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.10") {
    val input =
      """
        |def f(ys: List[Int32]): List[Int32] =
        | foreach (x = 1; y <- ys) println(y)
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.11") {
    val input =
      """
        |def f(): List[Int32] =
        | forM (x = 11) yield x
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.12") {
    val input =
      """
        |def f(ys: List[Int32]): List[Int32] =
        | forM (x = 2; y <- ys) yield y
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalForFragment](result)
  }

  test("IllegalForFragment.13") {
    val input =
      """
        |def f(x: Int32): List[Int32] =
        | foreach () yield 1
        |
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyForFragment](result)
  }

  test("IllegalFormalParamAscription.01") {
    val input =
      """
        |def f(): String =
        |    run ??? with handler Fail {
        |        def fail(x: String) = "hello"
        |    }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFormalParamAscription](result)
  }

  test("IllegalFormalParamAscription.02") {
    val input =
      """
        |def f(): String =
        |    run ??? with handler Fail {
        |        def fail(x: a) = "hello"
        |    }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFormalParamAscription](result)
  }

  test("IllegalFormalParamAscription.03") {
    val input =
      """
        |def f(): String =
        |    run ??? with handler Fail {
        |        def fail(_: Int32) = "hello"
        |    }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalFormalParamAscription](result)
  }

  test("IllegalLatticeProvenance.01") {
    val input =
      """
        |def main(): Unit  =
        |    let p = #{
        |        A(x; y) :- B(x, y).
        |    };
        |    let _ = pquery p select A(1; 2) with { B };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalLatticeProvenance](result)
  }

  test("IllegalLatticeProvenance.02") {
    val input =
      """
        |def main(): Unit  =
        |    let p = #{
        |        A(x; y) :- B(x, y).
        |    };
        |    let _ = pquery p select A("hello"; 2) with { B };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalLatticeProvenance](result)
  }

  test("IllegalLatticeProvenance.03") {
    val input =
      """
        |def main(): Unit  =
        |    let p = #{ };
        |    let _ = pquery p select A("hello"; 2) with { B };
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[WeederError.IllegalLatticeProvenance](result)
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

  test("IllegalModifier.05") {
    val input =
      """instance Sub[String] {
        |    pub override redef sub(x: String, y: String): String = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalModifier](result)
  }

  test("MissingTypematchBody.01") {
    val input =
      s"""
         |def f(): Int32 = typematch
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
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

  test("IllegalNullPattern.02") {
    val input =
      s"""
         |def f(): Int32 = match null {
         |    case _    => 456
         |    case null => 123
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalNullPattern.03") {
    val input =
      s"""
         |def f(): Int32 = match 0 {
         |    case null => 123
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalNullPattern.04") {
    val input =
      s"""
         |def f(): Int32 = match 0 {
         |    case { null | null } => 123
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNullPattern](result)
  }

  test("IllegalBigDecimalPattern.01") {
    val input =
      s"""
         |def f(): Int32 = match 123.456ff {
         |    case 123.456ff => 123
         |    case _         => 456
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalBigDecimalPattern](result)
  }

  test("IllegalBigDecimalPattern.02") {
    val input =
      s"""
         |def f(): Int32 = match 0.0ff {
         |    case _         => 456
         |    case 123.456ff => 123
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalBigDecimalPattern](result)
  }

  test("IllegalBigDecimalPattern.03") {
    val input =
      s"""
         |def f(): Int32 = match 0 {
         |    case 123.456ff => 123
         |}
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalBigDecimalPattern](result)
  }

  test("IllegalPrivateDeclaration.01") {
    val input =
      """
        |trait C[a] {
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

  test("IllegalPrivateDeclaration.03") {
    val input =
      """
        |instance C[Int32] {
        |    pub def f(): Int32 = 1
        |    def g(): Int32 = 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalPrivateDeclaration](result)
  }

  test("IllegalQualifiedExtPattern.01") {
    val input =
      """
        |def f(): Int32 = ematch xvar B(1) {
        |    case A.B(_) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalQualifiedExtPattern](result)
  }

  test("IllegalQualifiedExtPattern.02") {
    val input =
      """
        |def f(): Int32 = ematch xvar B(1) {
        |    case A.B.C(_) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalQualifiedExtPattern](result)
  }

  test("IllegalQualifiedExtPattern.03") {
    val input =
      """
        |def f(): Int32 = ematch xvar B(1) {
        |    case A.B(_) => 1
        |    case A.B.C(_) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalQualifiedExtPattern](result)
  }

  test("IllegalQualifiedExtPattern.04") {
    val input =
      """
        |def f(): Int32 = ematch xvar B(1) {
        |    case A(_) => 1
        |    case A.B(_) => 1
        |    case A.B.C(_) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalQualifiedExtPattern](result)
  }

  test("IllegalQualifiedExtPattern.05") {
    val input =
      """
        |def f(): Int32 = ematch xvar B(1) {
        |    case A(_) => 1
        |    case B(_) => 1
        |    case A.B(_) => 1
        |    case A.B.C(_) => 1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalQualifiedExtPattern](result)
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

  test("IllegalRecordExtensionPattern.02") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { x | { y | r } } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordExtensionPattern](result)
  }

  test("IllegalRecordExtensionPattern.03") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { x | { r = 1 } } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordExtensionPattern](result)
  }

  test("IllegalRecordExtensionPattern.04") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { x | null } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordExtensionPattern](result)
  }

  test("IllegalRecordExtensionPattern.05") {
    val input =
      """
        |def f(): Int32 = match { x = 1 } {
        |    case { x = { x = 2 | { } } } => 42
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordExtensionPattern](result)
  }

  test("IllegalRecordOperation.01") {
    val input =
      """
        |def f(): Int32 =
        |    let _ = { +x = 2 };
        |    2
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordOperation](result)
  }

  test("IllegalRecordOperation.02") {
    val input =
      """
        |def f(): Int32 =
        |    let _ = { -x };
        |    2
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordOperation](result)
  }

  test("IllegalRecordOperation.03") {
    val input =
      """
        |def f(): Int32 =
        |    let _ = { x = 3, +x = 4 };
        |    2
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalRecordOperation](result)
  }

  test("IllegalSelectChannelRuleFunctionCall.01") {
    val input =
      """
        |def f(): Int32 = select {
        |    case x <- NotChannel.NotRecv(a) => ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnexpectedSelectChannelRuleFunction](result)
  }

  test("IllegalTraitConstraintParameter.01") {
    val input =
      """
        |trait C[a] with D[Int32]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalTraitConstraintParameter](result)
  }

  test("IllegalTraitConstraintParameter.02") {
    val input =
      """
        |instance C[a] with D[Some[a]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalTraitConstraintParameter](result)
  }

  test("IllegalTraitConstraintParameter.03") {
    val input =
      """
        |instance C[a] with C[C[C[String]]]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalTraitConstraintParameter](result)
  }

  test("IllegalTraitParamNumber.01") {
    val input =
      """
        |trait A[a, b] {
        | def f(x: a): a
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalNumberOfTraitParameters](result)
  }

  test("IllegalInlineAndDontInlineAnnotation.01") {
    val input =
      """
        |@Inline @DontInline
        |def f(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InlineAndDontInline](result)
  }

  test("IllegalInlineAndDontInlineAnnotation.02") {
    val input =
      """
        |@DontInline @Inline
        |def f(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.InlineAndDontInline](result)
  }

  test("MalformedInt16.01") {
    val input = "def f(): Int16 = -100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt16.02") {
    val input = "def f(): Int16 = 100000i16"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt32.01") {
    val input = "def f(): Int32 = -10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt32.02") {
    val input = "def f(): Int32 = 10000000000i32"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt64.01") {
    val input = "def f(): Int64 = -100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt64.02") {
    val input = "def f(): Int64 = 100000000000000000000i64"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt8.01") {
    val input = "def f(): Int8 = -1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedInt8.02") {
    val input = "def f(): Int8 = 1000i8"
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedInt](result)
  }

  test("MalformedRegex.01") {
    val input =
      """
        |def f(): Regex = regex"[a-*"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("MalformedRegex.02") {
    val input =
      """
        |def f(): Regex = regex"a{}"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("MalformedRegex.03") {
    val input =
      """
        |def f(): Regex = regex"a{-1}"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("MalformedRegex.04") {
    val input =
      """
        |def f(): Regex = regex"\\p{InvalidGroupName}*"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedRegex](result)
  }

  test("MismatchedTypeParameters.01") {
    val input =
      """
        |enum E[a, b: Bool] {
        |    case E1
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("MismatchedTypeParameters.02") {
    val input =
      """
        |type alias T[a, b: Bool] = Int32
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("MismatchedTypeParameters.03") {
    val input =
      """
        |enum T[a, b: Bool](Int32)
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MismatchedTypeParameters](result)
  }

  test("MissingArgumentList.01") {
    val input =
      """
        |struct Person[r] {
        |    name: String,
        |    age: Int32
        |}
        |
        |def f(): Person[r] = new Per
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingArgumentList](result)
  }

  test("MissingArgumentList.02") {
    val input =
      """
        |struct Person[r] {
        |    name: String,
        |    age: Int32
        |}
        |
        |def f(): Person[r] = new Person
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingArgumentList](result)
  }

  test("MissingFormalParamAscription.01") {
    val input =
      """
        |def f(x): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingFormalParamAscription](result)
  }

  test("MissingFormalParamAscription.02") {
    val input =
      """
        |def f(x: Int32, y): Int32 = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingFormalParamAscription](result)
  }

  test("MissingFormalParamAscription.03") {
    val input =
      """
        |instance A[Int32] {
        |    pub def f(x): Int32 = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingFormalParamAscription](result)
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
        |trait C[a] {
        |    def f[b](x: b): a = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingTypeParamKind](result)
  }

  test("MissingTypeParamKind.03") {
    val input =
      """
        |trait A[a: Type] {
        |    pub def f[a](x: a): Int32 = ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MissingTypeParamKind](result)
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

  test("NonLinearPattern.10") {
    val input =
      """def f(): Bool = ematch xvar A(1) {
        |    case A(x, x) => true
        |}
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonLinearPattern](result)
  }

  test("NonUnaryAssocType.01") {
    val input =
      """
        |trait C[a] {
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

  test("NonUnaryAssocType.03") {
    val input =
      """
        |instance A[a] {
        |    type S[b, c, d] = Int32
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.NonUnaryAssocType](result)
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

  test("UndefinedAnnotation.03") {
    val input =
      """@Tests
        |def foo(x: Int32): Int32 = 42
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedAnnotation](result)
  }

  test("UndefinedIntrinsic.01") {
    val input =
      """
        |def f(): Unit = %%NOT_A_VALID_INTRINSIC%%()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedIntrinsic](result)
  }

  test("UndefinedIntrinsic.02") {
    val input =
      """
        |def f(): Unit = %%BOOLNOT%%()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedIntrinsic](result)
  }

  test("UndefinedIntrinsic.03") {
    val input =
      """
        |def f(): Unit = %%ARRAY_NEW%%(%%ARRAYNEW%%())
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UndefinedIntrinsic](result)
  }

  // Testing that the casing of 'foo' and 'Foo' should match
  test("IllegalUse.Alias.01") {
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

  test("IllegalUse.Alias.02") {
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

  test("IllegalUse.Alias.03") {
    val input =
      """
        |mod N {
        |    use M.{E => e}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  test("IllegalUse.Alias.04") {
    val input =
      """
        |mod B {
        |    use M.{e => A}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.IllegalUse](result)
  }

  test("UnqualifiedUse.01") {
    val input =
      """
        |use g
        |
        |def f(): String = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnqualifiedUse](result)
  }

  test("UnqualifiedUse.02") {
    val input =
      """
        |def f(): String = {
        |  use g;
        |  ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnqualifiedUse](result)
  }

  test("UnqualifiedUse.03") {
    val input =
      """
        |def f(): String = {
        |  def g() = {
        |      use g;
        |      ???
        |  };
        |  ???
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.UnqualifiedUse](result)
  }

  test("MalformedChar.Char.01") {
    val input =
      """
        |def f(): Char = 'ab'
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedChar](result)
  }

  test("MalformedChar.Char.02") {
    val input =
      """
        |def f(): Char = ''
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedChar](result)
  }

  test("MalformedChar.Pattern.Char.01") {
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

  test("MalformedUnicodeEscapeSequence.Char.01") {
    val input =
      """
        |def f(): Char = 'BSuINVALID'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscapeSequence.Char.02") {
    val input =
      """
        |def f(): Char = 'BSu000'
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscapeSequence.Interpolation.01") {
    val input =
      """
        |def f(): String = "${25}BSuINVALID"
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscapeSequence.Interpolation.02") {
    val input =
      """
        |def f(): String = "${25}BSu000"
        |""".stripMargin.replace("BS", "\\")
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.MalformedUnicodeEscapeSequence](result)
  }

  test("MalformedUnicodeEscapeSequence.Pattern.Char.01") {
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

  test("MalformedUnicodeEscapeSequence.Pattern.Char.02") {
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

  test("MalformedUnicodeEscapeSequence.Pattern.String.01") {
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

  test("MalformedUnicodeEscapeSequence.Pattern.String.02") {
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

  test("MalformedUnicodeEscapeSequence.String.01") {
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

  test("MissingCatchRules.01") {
    val input =
      """
        |def foo(): Bool =
        |    try { true } catch {}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingTypeParameter.Enum.01") {
    val input =
      """
        |enum E[]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingTypeParameter.Struct.01") {
    val input =
      """
        |struct S[] { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingTypeParameter.TypeAlias.01") {
    val input =
      """
        |type alias T[] = Int32
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingUse.Top.01") {
    val input =
      """
        |use Foo.{}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingUse.Mod.01") {
    val input =
      """
        |mod M {
        |  use Foo.{}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingUse.Def.01") {
    val input =
      """
        |def foo(): String = {
        |  use Foo.{}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingImport.Top.01") {
    val input =
      """
        |import java.lang.{}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("MissingImport.Mod.01") {
    val input =
      """
        |mod M {
        |  import java.lang.{}
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("EmptyParPatterns.01") {
    val input =
      """
        |def f(): Int32 = par () yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("EmptyTypeParamList.01") {
    val input =
      """
        |def foo[](): Bool = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[WeederError.EmptyTypeParamList](result)
  }

  test("EmptyEffectSet.01") {
    val input =
      """
        |def without01(): Bool = ??? without { }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

  test("EmptyEnumCaseType.01") {
    val input =
      """
        |enum E {
        |    case C()
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[ParseError.NeedAtleastOne](result)
  }

}
