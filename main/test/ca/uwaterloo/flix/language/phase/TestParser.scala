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
import ca.uwaterloo.flix.api.{Flix, RuleException}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.runtime.Model
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestParser extends FunSuite with TestUtils {

  /**
    * Runs Flix on the given input string `s`.
    */
  def run(s: String): Model = {
    new Flix().setOptions(Options.DefaultTest).addStr(s).solve().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Declarations                                                            //
  /////////////////////////////////////////////////////////////////////////////
  test("Declaration.Namespace.01") {
    val input =
      """namespace a {
        |  // comment
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.02") {
    val input =
      """namespace a.b.c {
        |  // comment
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.03") {
    val input =
      """namespace a {
        |  namespace b {
        |    namespace c {
        |      // comment
        |    }
        |  }
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.04") {
    val input =
      """namespace a.b.c {
        |  namespace d.e.f {
        |    namespace h.i.j {
        |      // comment
        |    }
        |  }
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.05") {
    val input =
      """namespace a {
        |  namespace b {
        |    namespace c {
        |      namespace a.b.c {
        |        def f(x: Int): Int = x + 42
        |      }
        |    }
        |  }
        |}
        |
        |def g: Int = a.b.c.a.b.c/f(21)
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.06") {
    val input =
      """namespace a {
        |  namespace b.c {
        |    namespace d {
        |      namespace e.f.g {
        |        def h(x: Int): Int = x + 42
        |      }
        |    }
        |  }
        |}
        |
        |def j: Int = a.b.c.d.e.f.g/h(21)
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.07") {
    val input =
      """namespace a {
        |  namespace b {
        |    namespace c {
        |      def f(x: Int): Int = x + 42
        |    }
        |  }
        |}
        |
        |namespace a.b.c {
        |  def g: Int = f(21)
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.08") {
    val input =
      """namespace a {
        |  namespace b {
        |    namespace c {
        |      def f(x: Int): Int = x + 42
        |      def g: Int = a.b.c/f(21)
        |    }
        |  }
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Namespace.09") {
    val input =
      """namespace a {
        |  namespace b {
        |    namespace c {
        |      def u(x: Int): Int = x + 42
        |    }
        |  }
        |
        |  namespace b.c {
        |    def v(x: Int): Int = x + 21
        |  }
        |}
        |
        |namespace a.b.c {
        |  def w(x: Int): Int = x + 11
        |}
        |
        |def r: Int = a.b.c/u(1) + a.b.c/v(2) + a.b.c/w(3)
      """.stripMargin
    run(input)
  }

  test("Declaration.Definition.01") {
    val input = "def f: Int = 42"
    run(input)
  }

  test("Declaration.Definition.02") {
    val input = "def f(x: Int): Int = x + 42"
    run(input)
  }

  test("Declaration.Definition.03") {
    val input = "def f(x: Int, y: Int): Int = x + y + 42"
    run(input)
  }

  test("Declaration.Definition.04") {
    val input = "def f(x: Int, y: Int, z: Int): Int = x + y + z + 42"
    run(input)
  }

  test("Declaration.Enum.01") {
    val input =
      """enum A {
        |  case B
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Enum.02") {
    val input =
      """enum A {
        |  case B(Int)
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Enum.03") {
    val input =
      """enum A {
        |  case B,
        |  case C(Int),
        |  case D(Bool, Int, Str)
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Enum.04") {
    val input =
      """enum A[a] {
        |  case B
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Enum.05") {
    val input =
      """enum A[a, b, c] {
        |  case A(a),
        |  case B(b),
        |  case C(c),
        |  case D(a, b, c)
        |}
      """.stripMargin
    run(input)
  }

  test("Declaration.Relation.01") {
    val input = "rel R(a: Int)"
    run(input)
  }

  test("Declaration.Relation.02") {
    val input = "rel R(a: Char, b: Int, c: Str)"
    run(input)
  }

  test("Declaration.Relation.03") {
    val input = "rel R(a: Int8, b: Int16, c: Int32, d: Int64)"
    run(input)
  }

  test("Lattice.01") {
    val input = "lat L(a: A)"
    expectError[ResolutionError.UndefinedType](new Flix().addStr(input).compile())
  }

  test("Lattice.02") {
    val input = "lat L(a: A, b: B, c: C)"
    expectError[ResolutionError.UndefinedType](new Flix().addStr(input).compile())
  }

  test("Declaration.Index.01") {
    val input =
      """rel R(a: Int)
        |index R({a})
      """.stripMargin
    run(input)
  }

  test("Declaration.Index.02") {
    val input =
      """rel R(a: Char, b: Int)
        |index R({a}, {b})
      """.stripMargin
    run(input)
  }

  test("Declaration.Index.03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |index R({a}, {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}, {a, b, c}, {a, b, c, d})
      """.stripMargin
    run(input)
  }

  test("Declaration.Class.01") {
    val input =
      """class Eq[A] {
        |  def eq(x: A, y: B): Bool
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Class.02") {
    val input =
      """class Coerce[A, B] {
        |  def coerce(a: A): B
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Class.03") {
    val input =
      """class Ord[A] => Eq[A] {
        |  def eq(x: A, y: A): Bool
        |  def lessEq(x: A, y: A): Bool
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Class.04") {
    val input =
      """class Eq[A] => PartialOrd[A], PreOrd[A] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Law.01") {
    val input = "law f: Bool = true"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Law.02") {
    val input = "law f(x: Int): Bool = x % 2 == 0"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Law.03") {
    val input = "law f(x: Int, y: Int): Bool = x > y"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Impl.01") {
    val input =
      """impl Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Impl.02") {
    val input =
      """impl Eq[(Int, Int)] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Impl.03") {
    val input =
      """impl Ord[Int] <= Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Impl.04") {
    val input =
      """impl A[Int, Int] <= B[Int], C[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Declaration.Fact.01") {
    val input =
      """
        |rel R(a: Int)
        |R(42).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.02") {
    val input =
      """
        |rel R(a: Char, b: Int)
        |R('a', 42).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.03") {
    val input =
      """
        |rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |R(1i8, 2i16, 3i32, 4i64).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.01") {
    val input =
      """
        |rel R(a: Int)
        |
        |R(x) :- R(x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.02") {
    val input =
      """
        |rel R(a: Int)
        |
        |R(x) :- R(x), R(x), R(x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.03") {
    val input =
      """
        |rel R(a: Int, b: Int)
        |
        |R(x, y) :- R(x, y), R(y, x).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.04") {
    val input =
      """def f: Int = 42
        |
        |rel R(a: Int)
        |
        |R(f()).
      """.stripMargin
    run(input)
  }

  test("Declaration.Fact.Head.True") {
    val input = "true."
    run(input)
  }

  test("Declaration.Fact.Head.False") {
    intercept[RuleException] {
      val input = "false."
      run(input)
    }
  }

  test("Declaration.Rule.Head.True") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |true :- R(x, y).
      """.stripMargin
    run(input)
  }

  test("Declaration.Rule.Head.False") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |false :- R(x, y).
      """.stripMargin
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unit.01") {
    val input = "def f: Unit = ()"
    run(input)
  }

  test("Expression.Bool.01") {
    val input = "def f: Bool = true"
    run(input)
  }

  test("Expression.Bool.02") {
    val input = "def f: Bool = false"
    run(input)
  }

  test("Expression.Char.01") {
    val input = "def f: Char = 'a'"
    run(input)
  }

  test("Expression.Char.02") {
    val input = "def f: Char = 'x'"
    run(input)
  }

  test("Expression.Float32.01") {
    val input = "def f: Float32 = 123.456f32"
    run(input)
  }

  test("Expression.Float32.02") {
    val input = "def f: Float32 = -123.456f32"
    run(input)
  }

  test("Expression.Float64.01") {
    val input = "def f: Float64 = 123.456f64"
    run(input)
  }

  test("Expression.Float64.02") {
    val input = "def f: Float64 = -123.456f64"
    run(input)
  }

  test("Expression.Int8.01") {
    val input = "def f: Int8 = 123i8"
    run(input)
  }

  test("Expression.Int8.02") {
    val input = "def f: Int8 = -123i8"
    run(input)
  }

  test("Expression.Int16.01") {
    val input = "def f: Int16 = 123i16"
    run(input)
  }

  test("Expression.Int16.02") {
    val input = "def f: Int16 = -123i16"
    run(input)
  }

  test("Expression.Int32.01") {
    val input = "def f: Int32 = 123i32"
    run(input)
  }

  test("Expression.Int32.02") {
    val input = "def f: Int32 = -123i32"
    run(input)
  }

  test("Expression.Int64.01") {
    val input = "def f: Int64 = 123i64"
    run(input)
  }

  test("Expression.Int64.02") {
    val input = "def f: Int64 = -123i64"
    run(input)
  }

  test("Expression.BigInt.01") {
    val input = "def f: BigInt = 123ii"
    run(input)
  }

  test("Expression.BigInt.02") {
    val input = "def f: BigInt = -123ii"
    run(input)
  }

  test("Expression.Str.01") {
    val input = "def f: Str = \"\""
    run(input)
  }

  test("Expression.Str.02") {
    val input = "def f: Str = \"a\""
    run(input)
  }

  test("Expression.Str.03") {
    val input = "def f: Str = \"0\""
    run(input)
  }

  test("Expression.Str.04") {
    val input = "def f: Str = \"abc\""
    run(input)
  }

  test("Expression.Str.05") {
    val input = "def f: Str = \"abc123'!@#$%^&*()\""
    run(input)
  }

  test("Expression.LogicalExp.01") {
    val input = "def f: Bool = true && false"
    run(input)
  }

  test("Expression.LogicalExp.02") {
    val input = "def f: Bool = true || false"
    run(input)
  }

  test("Expression.LogicalExp.03") {
    val input = "def f: Bool = 1 < 2 && 3 < 4"
    run(input)
  }

  test("Expression.ComparisonExp.01") {
    val input = "def f: Bool = 1 < 2"
    run(input)
  }

  test("Expression.ComparisonExp.02") {
    val input = "def f: Bool = 1 + 2 > 3"
    run(input)
  }

  test("Expression.ComparisonExp.03") {
    val input = "def f: Bool = 1 + 2 > 3 + 4"
    run(input)
  }

  test("Expression.MultiplicativeExp.01") {
    val input = "def f: Int = 1 * 2"
    run(input)
  }

  test("Expression.MultiplicativeExp.02") {
    val input = "def f: Int = 1 * 2 * 3"
    run(input)
  }

  test("Expression.MultiplicativeExp.03") {
    val input = "def f: Int = 1 * 2 + 3"
    run(input)
  }

  test("Expression.MultiplicativeExp.04") {
    val input = "def f: Int = 1 + 2 * 3"
    run(input)
  }

  test("Expression.AdditiveExp.01") {
    val input = "def f: Int = 1 + 2"
    run(input)
  }

  test("Expression.AdditiveExp.02") {
    val input = "def f: Int = 1 + 2 + 3"
    run(input)
  }

  test("Expression.AdditiveExp.03") {
    val input = "def f: Int = 1 - 2"
    run(input)
  }

  test("Expression.AdditiveExp.04") {
    val input = "def f: Int = 1 - 2 - 3"
    run(input)
  }

  test("Expression.AdditiveExp.05") {
    val input = "def f: Int = 1 + 2 - 3 + 4 - 5 + 6"
    run(input)
  }

  test("Expression.Infix.01") {
    val input =
      """def add(x: Int, y: Int): Int = x + y
        |
        |def f: Int = 1 `add` 2
      """.stripMargin
    val model = run(input)
    assertResult(3)(model.getConstant("f"))
  }

  // TODO
  ignore("Expression.Infix.02") {
    val input =
      """def add(x: Int, y: Int): Int = x + y
        |def sub(x: Int, y: Int): Int = x - y
        |def mul(x: Int, y: Int): Int = x * y
        |
        |def f: Int = 1 `add` 2 `sub` 3 `mul` 4
      """.stripMargin
    val model = run(input)
    assertResult(0)(model.getConstant("f"))
  }

  test("Expression.Postfix.01") {
    val input =
      """def abs(x: Int): Int = if(x >= 0) x else -x
        |
        |def f: Int = 1.abs()
      """.stripMargin
    val model = run(input)
    assertResult(1)(model.getConstant("f"))
  }

  test("Expression.Postfix.02") {
    val input =
      """def abs(x: Int): Int = if(x >= 0) x else -x
        |def inc(x: Int): Int = x + 1
        |def dec(x: Int): Int = x - 1
        |
        |def f: Int = 1.abs().inc().dec()
      """.stripMargin
    val model = run(input)
    assertResult(1)(model.getConstant("f"))
  }

  test("Expression.Postfix.03") {
    val input =
      """def add(x: Int, y: Int): Int = x + y
        |
        |def f: Int = 1.add(2)
      """.stripMargin
    val model = run(input)
    assertResult(3)(model.getConstant("f"))
  }

  test("Expression.Postfix.04") {
    val input =
      """def add(x: Int, y: Int): Int = x + y
        |def sub(x: Int, y: Int): Int = x - y
        |def mul(x: Int, y: Int): Int = x * y
        |
        |def f: Int = 1.add(2).sub(3).mul(4)
      """.stripMargin
    run(input)
    val model = run(input)
    assertResult(0)(model.getConstant("f"))
  }

  test("Expression.Postfix.05") {
    val input =
      """def add(x: Int, y: Int, z: Int): Int = x + y + z
        |
        |def f: Int = 1.add(2, 3)
      """.stripMargin
    val model = run(input)
    assertResult(6)(model.getConstant("f"))
  }

  test("Expression.Postfix.06") {
    val input =
      """def add(x: Int, y: Int, z: Int): Int = x + y + z
        |def sub(x: Int, y: Int, z: Int): Int = x - y - z
        |def mul(x: Int, y: Int, z: Int): Int = x * y * z
        |
        |def f: Int = 1.add(2, 3).sub(4, 5).mul(6, 7)
      """.stripMargin
    run(input)
  }

  test("Expression.UnaryExp.01") {
    val input = "def f: Int = +1"
    run(input)
  }

  test("Expression.UnaryExp.02") {
    val input = "def f: Int = -1"
    run(input)
  }

  test("Expression.UnaryExp.03") {
    val input = "def f: Int = ~1"
    run(input)
  }

  test("Expression.UnaryExp.04") {
    val input = "def f: Bool = !!true"
    run(input)
  }

  test("Expression.Ascribe.01") {
    val input = "def f: Bool = true: Bool"
    run(input)
  }

  test("Expression.Block.01") {
    val input = "def f: Int = {42}"
    run(input)
  }

  test("Expression.Block.02") {
    val input = "def f: Int = {{42}}"
    run(input)
  }

  test("Expression.Block.03") {
    val input = "def f: Int = {{{42}}}"
    run(input)
  }

  test("Expression.Block.04") {
    val input =
      """
        |def f: Int = {
        |  let x = {
        |    let y = 42;
        |    y
        |  };
        |  x + 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Block.05") {
    val input =
      """
        |def f: Int = {
        |  let x = {
        |    let y = 42;
        |    let z = 21;
        |    y + z
        |  };
        |  x + 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Block.06") {
    val input =
      """
        |def f: Int = {
        |  let x = {
        |    let y = 42;
        |    let z = 21;
        |    y + z
        |  };
        |  let w = {
        |    let y = 84;
        |    let z = 11;
        |    y + z
        |  };
        |  x + w
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Block.07") {
    val input =
      """
        |def f: Int = {
        |  let x = {42};
        |  let y = {21};
        |  x + y
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Block.08") {
    val input =
      """
        |def f: Int = {
        |  let x = {42};
        |  let y = {
        |    let z = 21;
        |    let w = {11};
        |      z + w
        |  };
        |  x + y
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Block.09") {
    val input =
      """
        |def f: Int = {
        |  let x = {42};
        |  let y = {
        |    let z = 21;
        |    let w = {
        |      let q = 11;
        |      let u = 5;
        |      q + u
        |    };
        |    z + w
        |  };
        |  x + y
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.01") {
    val input =
      """
        |def f: Int =
        |  let x = 42;
        |    x
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.02") {
    val input =
      """
        |def f: Int =
        |  let (x, y) = (42, 21);
        |    x + y
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.03") {
    val input =
      """
        |def f: Int =
        |  let x = 1;
        |  let y = 2;
        |  let z = 3;
        |    x + y + z
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.04") {
    // Note: This is to test the performance of deeply nested lets.
    val input =
      """
        |def f: Int =
        |    let x1 = 1;
        |    let x2 = 1;
        |    let x3 = 1;
        |    let x4 = 1;
        |    let x5 = 1;
        |    let x6 = 1;
        |    let x7 = 1;
        |    let x8 = 1;
        |    let x9 = 1;
        |    let y1 = 1;
        |    let y2 = 1;
        |    let y3 = 1;
        |    let y4 = 1;
        |    let y5 = 1;
        |    let y6 = 1;
        |    let y7 = 1;
        |    let y8 = 1;
        |    let y9 = 1;
        |    let z1 = 1;
        |    let z2 = 1;
        |    let z3 = 1;
        |    let z4 = 1;
        |    let z5 = 1;
        |    let z6 = 1;
        |    let z7 = 1;
        |    let z8 = 1;
        |    let z9 = 1;
        |        1
      """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.05") {
    val input =
      """
        |def f: Int =
        |  let x = 42;
        |    x
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.06") {
    val input =
      """
        |def f: Int =
        |  let x = 42;
        |  let y = 21;
        |    x + y
        | """.stripMargin
    run(input)
  }

  test("Expression.LetMatch.07") {
    val input =
      """
        |def f: Int =
        |  let x = 1;
        |  let y = 2;
        |  let z = 3;
        |    x + y + z
      """.stripMargin
    run(input)
  }

  test("Expression.IfThenElse.01") {
    val input = "def f: Int = if (true) 42 else 21"
    run(input)
  }

  test("Expression.IfThenElse.02") {
    val input = "def f: Int = if ((true)) (1) else (2)"
    run(input)
  }

  test("Expression.IfThenElse.03") {
    val input = "def f: (Int, Int) = if (true || false) (1, 2) else (3, 4)"
    run(input)
  }

  test("Expression.Switch.01") {
    val input =
      """def f(x: Int): Int = switch {
        |  case true  => 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Switch.02") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Switch.03") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |  case x > 0  => 2
        |  case x == 0 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.01") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.02") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.03") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => match 3 with {
        |    case 4 => 5
        |  }
        |  case 6 => 7
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Match.04") {
    val input =
      """def f: Int = match
        |  match 1 with {
        |    case 2 => 3
        |  } with {
        |    case 4 => 5
        |}
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.01") {
    val input =
      """def f: Int = 42
        |def g: Int = f()
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.02") {
    val input =
      """def f(x: Int): Int = x
        |def g: Int = f(42)
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.03") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, 2)
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.04") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, f(2, 3))
      """.stripMargin
    run(input)
  }

  test("Expression.Apply.05") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(f(1, 2), f(3, 4))
      """.stripMargin
    run(input)
  }

  test("Expression.Enum.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f: Color = Red
      """.stripMargin
    run(input)
  }

  test("Expression.Enum.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    run(input)
  }

  test("Expression.Enum.03") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Square(Int, Int)
        |}
        |
        |def f: Shape = Circle(42)
        |def g: Shape = Square(21, 42)
      """.stripMargin
    run(input)
  }

  test("Expression.Enum.04") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Square(Int, Int)
        |}
        |
        |def f: Shape = Shape.Circle(42)
        |def g: Shape = Shape.Square(21, 42)
      """.stripMargin
    run(input)
  }

  test("Expression.Tuple.01") {
    val input = "def f: Unit = ()"
    run(input)
  }

  test("Expression.Tuple.02") {
    val input = "def f: Int = (42)"
    run(input)
  }

  test("Expression.Tuple.03") {
    val input = "def f: (Int, Int) = (42, 21)"
    run(input)
  }

  test("Expression.Tuple.04") {
    val input = "def f(x: Int): (Int, Int, Int) = (42, x, 21)"
    run(input)
  }

  test("Expression.Tuple.05") {
    val input = "def f(x: Int): (Int, (Int, Int), Int) = (42, (x, x), 21)"
    run(input)
  }

  test("Expression.List.01") {
    val input = "def f: List[Int] = Nil"
    run(input)
  }

  test("Expression.List.02") {
    val input = "def f: List[Int] = 1 :: Nil"
    run(input)
  }

  test("Expression.List.03") {
    val input = "def f: List[Int] = 1 :: 2 :: Nil"
    run(input)
  }

  test("Expression.List.04") {
    val input = "def f: List[(Int, Int)] = Nil"
    run(input)
  }

  test("Expression.List.05") {
    val input = "def f: List[(Int, Int)] = (1, 2) :: (3, 4) :: Nil"
    run(input)
  }

  test("Expression.ListList.01") {
    val input = "def f: List[List[Int]] = Nil"
    run(input)
  }

  test("Expression.ListList.02") {
    val input = "def f: List[List[Int]] = (1 :: Nil) :: Nil"
    run(input)
  }

  test("Expression.ListList.03") {
    val input = "def f: List[List[Int]] = (Nil) :: (1 :: Nil) :: (2 :: 3 :: 4 :: Nil) :: Nil"
    run(input)
  }

  test("Expression.Append.01") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = Nil ::: Nil"
    run(input + append)
  }

  test("Expression.Append.02") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = 1 :: Nil ::: 1 :: Nil"
    run(input + append)
  }

  test("Expression.Append.03") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = 1 :: Nil ::: 1 :: 2 :: Nil"
    run(input + append)
  }

  test("Expression.Append.04") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = 1 :: 2 :: Nil ::: 1 :: 2 :: Nil"
    run(input + append)
  }

  test("Expression.Append.05") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = Nil ::: Nil ::: Nil"
    run(input + append)
  }

  test("Expression.Append.06") {
    // TODO: Once list is included by default this can be improved.
    val append =
      """
        |namespace List {
        |    def append[a](xs: List[a], ys: List[a]): List[a] = ???
        |}
        |
      """.stripMargin

    val input = "def f: List[Int] = 1 :: Nil ::: 2 :: Nil ::: 3 :: Nil"
    run(input + append)
  }

  test("Expression.Vec.01") {
    val input = "def f: Vec[Int] = #[]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Vec.02") {
    val input = "def f: Vec[Int] = #[1]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Vec.03") {
    val input = "def f: Vec[Int] = #[1, 2]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Vec.04") {
    val input = "def f: Vec[Int] = #[1, 2, 3]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Vec.05") {
    val input = "def f: Vec[(Char, Int)] = #[]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Vec.06") {
    val input = "def f: Vec[(Char, Int)] = #[('a', 21), ('b', 42)]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.VecVec.01") {
    val input = "def f: Vec[Vec[Int]] = #[]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.VecVec.03") {
    val input = "def f: Vec[Vec[Int]] = #[#[], #[1], #[1, 2, 3]]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Set.01") {
    val input = "def f: Set[Int] = #{}"
    run(input)
  }

  test("Expression.Set.02") {
    val input = "def f: Set[Int] = #{1}"
    run(input)
  }

  test("Expression.Set.03") {
    val input = "def f: Set[Int] = #{1, 2}"
    run(input)
  }

  test("Expression.Set.04") {
    val input = "def f: Set[Int] = #{1, 2, 3}"
    run(input)
  }

  test("Expression.Set.05") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2)}"
    run(input)
  }

  test("Expression.Set.06") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2), (3, 4)}"
    run(input)
  }

  test("Expression.Set.07") {
    val input = "def f: Set[Int] = #{1 + 2, 3 + 4, 5 + 6}"
    run(input)
  }

  test("Expression.SetSet.01") {
    val input = "def f: Set[Set[Int]] = #{}"
    run(input)
  }

  test("Expression.SetSet.02") {
    val input = "def f: Set[Set[Int]] = #{#{}}"
    run(input)
  }

  test("Expression.SetSet.03") {
    val input = "def f: Set[Set[Int]] = #{#{1, 2}, #{3, 4}, #{5, 6}}"
    run(input)
  }

  test("Expression.Map.01") {
    val input = "def f: Map[Char, Int] = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Map.02") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Map.03") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Map.04") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2, 'c' -> 3}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Map.05") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Map.06") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{(1i8, 2i16) -> (3i32, 4i64)}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapMap.01") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapMap.02") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapMap.03") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}, 2 -> @{3 -> 'a', 4 -> 'b'}}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapList.01") {
    val input = "def f: Map[Int, List[Int]] = @{1 -> 2 :: 3 :: Nil, 4 -> 5 :: 6 :: Nil}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapListSet.01") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapListSet.02") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> Nil}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.MapListSet.04") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> #{1, 2, 3} :: #{4, 5, 6} :: Nil}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Var.01") {
    val input = "def f(x: Int): Int = x"
    run(input)
  }

  test("Expression.Lambda.01") {
    val input = "def f: Int -> Int = x -> x"
    run(input)
  }

  test("Expression.Lambda.02") {
    val input = "def f: Int -> Int = (x) -> x"
    run(input)
  }

  test("Expression.Lambda.03") {
    val input = "def f: (Bool, Char) -> Int = (x, y) -> 42"
    run(input)
  }

  test("Expression.Lambda.04") {
    val input = "def f: (Bool, Char, Int) -> Int = (x, y, z) -> 42"
    run(input)
  }

  test("Expression.Lambda.05") {
    val input = "def f: (Int8, Int16, Int32, Int64) -> Int32 = (x, y, z, w) -> z"
    run(input)
  }

  test("Expression.Lambda.06") {
    val input = "def f: Int -> (Bool, Char) = x -> (true, 'a')"
    run(input)
  }

  test("Expression.Lambda.07") {
    val input = "def f: Int -> (Bool, Char, Int) = x -> (true, 'a', 42)"
    run(input)
  }

  test("Expression.Lambda.08") {
    val input = "def f: (Bool, Char) -> (Char, Bool) = (x, y) -> (y, x)"
    run(input)
  }

  test("Expression.Lambda.09") {
    val input = "def f: (Bool, Char, Int) -> (Int, Char, Bool) = (x, y, z) -> (z, y, x)"
    run(input)
  }

  test("Expression.Lambda.10") {
    val input = "def f: ((Bool, Char), Int) -> (Bool, Char) = (x, y) -> x"
    run(input)
  }

  test("Expression.Lambda.11") {
    val input = "def f: (Bool, (Char, Int)) -> (Char, Int) = (x, y) -> y"
    run(input)
  }

  test("Expression.Lambda.12") {
    val input = "def f: (Int, Int) -> ((Int, Int), (Int, Int)) = (x, y) -> ((x, y), (y, x))"
    run(input)
  }

  test("Expression.Lambda.13") {
    val input = "def f: Bool -> Char -> Int = x -> (y -> 42)"
    run(input)
  }

  test("Expression.Lambda.14") {
    val input = "def f: (Bool, Bool) -> Char -> Int = (x1, x2) -> (y -> 42)"
    run(input)
  }

  test("Expression.Lambda.15") {
    val input = "def f: Bool -> (Char, Char) -> Int = x -> ((y1, y2) -> 42)"
    run(input)
  }

  test("Expression.Lambda.16") {
    val input = "def f: Bool -> Char -> (Int, Int) = x -> (y -> (21, 42))"
    run(input)
  }

  test("Expression.Lambda.17") {
    val input = "def f: (Bool, Bool) -> (Char, Char) -> (Int, Int) = (x1, x2) -> ((y1, y2) -> (21, 42))"
    run(input)
  }

  test("Expression.MatchLambda.01") {
    val input = "def f: Int -> Int = match x -> x"
    run(input)
  }

  test("Expression.MatchLambda.02") {
    val input = "def f: ((Int, Int)) -> Int = match (x, y) -> x + y"
    run(input)
  }

  test("Expression.MatchLambda.03") {
    val input = "def f: ((Int, Int, Int)) -> Int = match (x, y, z) -> x + y + z"
    run(input)
  }

  test("Expression.MatchLambda.04") {
    val input = "def f: (((Int, Int), (Int, Int))) -> Int = match ((x, y), (z, w)) -> x + y + z + w"
    run(input)
  }

  test("Expression.MatchLambda.05") {
    val input = "def f: Option[Int] -> Int = match None -> 42"
    run(input)
  }

  test("Expression.MatchLambda.06") {
    val input = "def f: Option[Int] -> Int = match Some(x) -> x"
    run(input)
  }

  test("Expression.MatchLambda.07") {
    val input = "def f: List[Int] -> Int = match Nil -> 42"
    run(input)
  }

  test("Expression.MatchLambda.08") {
    val input = "def f: List[Int] -> Int = match x :: Nil -> x"
    run(input)
  }

  test("Expression.MatchLambda.09") {
    val input = "def f: List[Int] -> Int = match x :: y :: Nil -> x + y"
    run(input)
  }

  test("Expression.Existential.01") {
    val input = "def f: Bool = ∃(x: Bool). true"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Existential.02") {
    val input = "def f: Bool = ∃(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Existential.03") {
    val input = "def f: Bool = \\exists(x: Bool). true"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Existential.04") {
    val input = "def f: Bool = \\exists(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Universal.01") {
    val input = "def f: Bool = ∀(x: Bool). true"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Universal.02") {
    val input = "def f: Bool = ∀(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Universal.03") {
    val input = "def f: Bool = \\forall(x: Bool). true"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Expression.Universal.04") {
    val input = "def f: Bool = \\forall(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Pattern.Wildcard.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case _ => 42
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Var.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case x => x
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Unit.01") {
    val input =
      """def f(x: Unit): Int = match x with {
        |  case () => 42
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Bool.01") {
    val input =
      """def f(x: Bool): Int = match x with {
        |  case true => 42
        |  case false => 21
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Char.01") {
    val input =
      """def f(x: Char): Int = match x with {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Float32.01") {
    val input =
      """def f(x: Float32): Int = match x with {
        |  case 1.0f32 => 1
        |  case 2.0f32 => 2
        |  case 3.0f32 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Float64.01") {
    val input =
      """def f(x: Float64): Int = match x with {
        |  case 1.0f64 => 1
        |  case 2.0f64 => 2
        |  case 3.0f64 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int8.01") {
    val input =
      """def f(x: Int8): Int = match x with {
        |  case 1i8 => 1
        |  case 2i8 => 2
        |  case 3i8 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int16.01") {
    val input =
      """def f(x: Int16): Int = match x with {
        |  case 1i16 => 1
        |  case 2i16 => 2
        |  case 3i16 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int32.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case 1 => 1
        |  case 2 => 2
        |  case 3 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Int64.01") {
    val input =
      """def f(x: Int64): Int = match x with {
        |  case 1i64 => 1
        |  case 2i64 => 2
        |  case 3i64 => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Red => 1
        |  case Blu => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu
        |}
        |
        |def f(x: Color): Int = match x with {
        |  case Color.Red => 1
        |  case Color.Blu => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.03") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Rectangle(Int, Int)
        |}
        |
        |def f(x: Shape): Int = match x with {
        |  case Circle(r) => r
        |  case Rectangle(h, w) => h * w
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Enum.04") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Rectangle(Int, Int)
        |}
        |
        |def f(x: Shape): Int = match x with {
        |  case Shape.Circle(r) => r
        |  case Shape.Rectangle(h, w) => h * w
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Tuple.01") {
    val input =
      """def f(x: (Bool, Char, Int)): Int = match x with {
        |  case (true, 'a', 42) => 1
        |  case (false, 'b', 21) => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.01") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.02") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: Nil => 0
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.03") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: Nil => 0
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.04") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: 3 :: Nil => 0
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.05") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: Nil => x
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.06") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: y :: Nil => x + y
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.07") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |  case x :: rs => 1 + f(rs)
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.08") {
    val input =
      """def f(xs: List[Int]): Bool = match xs with {
        |  case Nil => true
        |  case x :: y :: rs => f(rs)
        |  case _ => false
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.09") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |  case x :: Nil => x
        |  case x :: y :: Nil => x + y
        |  case xs => 42
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.10") {
    val input =
      """def f(xs: List[(Char, Int)]): Int = match xs with {
        |  case Nil => 0
        |  case (c, i) :: Nil => i
        |  case (c1, i1) :: (c2, i2) :: Nil => i1 + i2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.List.11") {
    val input =
      """def f(xs: List[(Char, Int)]): Int = match xs with {
        |  case Nil => 0
        |  case (c, 42) :: Nil => 1
        |  case ('a', i1) :: (c2, 21) :: Nil => 2
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.ListList.01") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: Nil) :: (y :: Nil) :: Nil => x + y
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.ListList.02") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: y :: Nil) :: (z :: w :: Nil) :: Nil => x + y + z + w
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.ListList.03") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: xs) :: (y :: ys) :: (z :: zs) :: Nil => x + y + z
        |}
      """.stripMargin
    run(input)
  }

  test("Pattern.Vec.01") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.02") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.03") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.04") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2, 3] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.05") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x] => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.06") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y] => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.07") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z] => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Vec.08") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z, rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.VecVec.01") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.VecVec.02") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[1, 2, 3], #[4, 5, 6]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.VecVec.03") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[], #[1], #[1, 2, 3], #[x, y, z], rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.01") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.02") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.03") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.04") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3, rs...} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.05") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.06") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.07") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y, z} => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.08") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{} => 0
        |  case #{x, y, z, rs...} => f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Set.09") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{} => 0
        |  case #{x} => x
        |  case #{x, rs...} => x + fs(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.SetSet.01") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.SetSet.02") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x}, #{y}, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.SetSet.03") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x, y, as...}, #{z, w, bs...}, rs...} => x + y + z + w
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.01") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.02") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.03") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.04") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21, c -> 11} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.05") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.06") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.07") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Pattern.Map.08") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{} => 0
        |  case @{'a' -> x} => x
        |  case @{'a' -> x, rs...} => f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Unit.01") {
    val input = "def f: Unit = ()"
    run(input)
  }

  test("Type.Bool.01") {
    val input = "def f: Bool = true"
    run(input)
  }

  test("Type.Bool.02") {
    val input = "def f: Bool = false"
    run(input)
  }

  test("Type.Char.01") {
    val input = "def f: Char = 'a'"
    run(input)
  }

  test("Type.Float32.01") {
    val input = "def f: Float32 = 0.0f32"
    run(input)
  }

  test("Type.Float64.01") {
    val input = "def f: Float64 = 0.0f64"
    run(input)
  }

  test("Type.Int8.01") {
    val input = "def f: Int8 = 0i8"
    run(input)
  }

  test("Type.Int16.01") {
    val input = "def f: Int16 = 0i16"
    run(input)
  }

  test("Type.Int32.01") {
    val input = "def f: Int32 = 0i32"
    run(input)
  }

  test("Type.Int64.01") {
    val input = "def f: Int64 = 0i64"
    run(input)
  }

  test("Type.BigInt.01") {
    val input = "def f: BigInt = 0ii"
    run(input)
  }

  test("Type.Str.01") {
    val input = "def f: Str = \"foobar\""
    run(input)
  }

  test("Type.Enum.01") {
    val input =
      """enum Color {
        |  case Red
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    run(input)
  }

  test("Type.Tuple.01") {
    val input = "def f: (Int, Int) = (1, 2)"
    run(input)
  }

  test("Type.Tuple.02") {
    val input = "def f: (Unit, Bool, Char, Int) = ((), true, 'a', 42)"
    run(input)
  }

  test("Type.Lambda.01") {
    val input = "def f: Bool -> Int = x -> 42"
    run(input)
  }

  test("Type.Lambda.02") {
    val input = "def f: (Bool, Char, Int) -> Str = (x,y, z) -> \"a\""
    run(input)
  }

  test("Type.Lambda.03") {
    val input = "def f: Str -> (Bool, Char, Int) = x -> (true, 'a', 42)"
    run(input)
  }

  test("Type.List.01") {
    val input = "def f: List[Int] = Nil"
    run(input)
  }

  test("Type.Vec.01") {
    val input = "def f: Vec[Int] = #[]"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Type.Set.01") {
    val input = "def f: Set[Int] = #{}"
    run(input)
  }

  test("Type.Map.01") {
    val input = "def f: Map[Int, Int] = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Type.Infix.01") {
    val input = "def f: Char `Map` Int = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  test("Type.Infix.02") {
    val input = "def f: Int `Map` Str = @{}"
    intercept[scala.NotImplementedError] {
      run(input)
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Ident.01") {
    val input = "def x: Int = 42"
    run(input)
  }

  test("Ident.02") {
    val input = "def xx: Int = 42"
    run(input)
  }

  test("Ident.03") {
    val input = "def xxx: Int = 42"
    run(input)
  }

  test("Ident.04") {
    val input = "def xY: Int = 42"
    run(input)
  }

  test("Ident.05") {
    val input = "def xxxYyy: Int = 42"
    run(input)
  }

  test("Ident.06") {
    val input = "def xxxYyyZzz: Int = 42"
    run(input)
  }

  test("Ident.07") {
    val input = "def x0: Int = 42"
    run(input)
  }

  test("Ident.08") {
    val input = "def x0123: Int = 42"
    run(input)
  }

  test("Ident.09") {
    val input = "def x_y_z: Int = 42"
    run(input)
  }

  test("Ident.10") {
    val input = "def x_Y32Y_15zz: Int = 42"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Names                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Names.Variable.Greek.01") {
    val input = "def f(α: Int): Int = α"
    run(input)
  }

  test("Names.Variable.Greek.02") {
    val input = "def f(β: Int): Int = β"
    run(input)
  }

  test("Names.Variable.Greek.03") {
    val input = "def f(ε: Int): Int = ε"
    run(input)
  }

  test("Names.Variable.Greek.04") {
    val input = "def f(κ: Int): Int = κ"
    run(input)
  }

  test("Names.Variable.Greek.05") {
    val input = "def f(σ: Int): Int = σ"
    run(input)
  }

  test("Names.Variable.Greek.06") {
    val input = "def f(Γ: Int): Int = Γ"
    run(input)
  }

  test("Names.Variable.Greek.07") {
    val input = "def f(Δ: Int): Int = Δ"
    run(input)
  }

  test("Names.Variable.Greek.08") {
    val input = "def f(Σ: Int): Int = Σ"
    run(input)
  }

  test("Names.Math.⊥") {
    val input = "def ⊥: Int = ???"
    run(input)
  }

  test("Names.Math.⊤") {
    val input = "def ⊤: Int = ???"
    run(input)
  }

  test("Names.Math.⊑") {
    val input = "def ⊑(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⊔") {
    val input = "def ⊔(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⊓") {
    val input = "def ⊓(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.∇") {
    val input = "def ∇(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.∆") {
    val input = "def ∆(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⊡") {
    val input = "def ⊡(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.∈") {
    val input = "def ∈(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⊕") {
    val input = "def ⊕(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⊗") {
    val input = "def ⊗(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.↪") {
    val input = "def ↪(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Math.⇥") {
    val input = "def ⇥(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.++") {
    val input = "def ++(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.--") {
    val input = "def --(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.|>") {
    val input = "def |>(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.<|") {
    val input = "def <|(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.<*>") {
    val input = "def <*>(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.<**>") {
    val input = "def <**>(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.*>") {
    val input = "def *>(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.<*") {
    val input = "def <*(x: Int, y: Int): Int = ???"
    run(input)
  }

  test("Names.Operator.|+|") {
    val input = "def |+|(x: Int, y: Int): Int = ???"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Literal.Unit.01") {
    val input = "def f: Unit = ()"
    run(input)
  }

  test("Literal.True.01") {
    val input = "def f: Bool = true"
    run(input)
  }

  test("Literal.False.01") {
    val input = "def f: Bool = false"
    run(input)
  }

  test("Literal.Char.01") {
    val input = "def f: Char = 'a'"
    run(input)
  }

  test("Literal.Float32.01") {
    val input = "def f: Float32 = 123.456f32"
    run(input)
  }

  test("Literal.Float32.02") {
    val input = "def f: Float32 = +123.456f32"
    run(input)
  }

  test("Literal.Float32.03") {
    val input = "def f: Float32 = -123.456f32"
    run(input)
  }

  test("Literal.Float64.01") {
    val input = "def f: Float64 = 123.456f64"
    run(input)
  }

  test("Literal.Float64.02") {
    val input = "def f: Float64 = +123.456f64"
    run(input)
  }

  test("Literal.Float64.03") {
    val input = "def f: Float64 = -123.456f64"
    run(input)
  }

  test("Literal.Int8.01") {
    val input = "def f: Int8 = 123i8"
    run(input)
  }

  test("Literal.Int8.02") {
    val input = "def f: Int8 = +123i8"
    run(input)
  }

  test("Literal.Int8.03") {
    val input = "def f: Int8 = -123i8"
    run(input)
  }

  test("Literal.Int16.01") {
    val input = "def f: Int16 = 123i16"
    run(input)
  }

  test("Literal.Int16.02") {
    val input = "def f: Int16 = +123i16"
    run(input)
  }

  test("Literal.Int16.03") {
    val input = "def f: Int16 = -123i16"
    run(input)
  }

  test("Literal.Int32.01") {
    val input = "def f: Int32 = 123i32"
    run(input)
  }

  test("Literal.Int32.02") {
    val input = "def f: Int32 = +123i32"
    run(input)
  }

  test("Literal.Int32.03") {
    val input = "def f: Int32 = -123i32"
    run(input)
  }

  test("Literal.Int64.01") {
    val input = "def f: Int64 = 123i64"
    run(input)
  }

  test("Literal.Int64.02") {
    val input = "def f: Int64 = +123i64"
    run(input)
  }

  test("Literal.Int64.03") {
    val input = "def f: Int64 = -123i64"
    run(input)
  }

  test("Literal.BigInt.01") {
    val input = "def f: BigInt = 123ii"
    run(input)
  }

  test("Literal.BigInt.02") {
    val input = "def f: BigInt = +123ii"
    run(input)
  }

  test("Literal.BigInt.03") {
    val input = "def f: BigInt = -123ii"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("Operator.Unary !") {
    val input = "def f(b: Bool): Bool = !b"
    run(input)
  }

  test("Operator.Unary +") {
    val input = "def f(i: Int): Int = +i"
    run(input)
  }

  test("Operator.Unary -") {
    val input = "def f(i: Int): Int = -i"
    run(input)
  }

  test("Operator.Unary ~") {
    val input = "def f(i: Int): Int = ~i"
    run(input)
  }

  test("Operator.Binary.LogicalOp &&") {
    val input = "def f(x: Bool, y: Bool): Bool = x && y"
    run(input)
  }

  test("Operator.Binary.LogicalOp ||") {
    val input = "def f(x: Bool, y: Bool): Bool = x || y"
    run(input)
  }

  test("Operator.Binary.Bitwise &") {
    val input = "def f(x: Int, y: Int): Int = x & y"
    run(input)
  }

  test("Operator.Binary.Bitwise |") {
    val input = "def f(x: Int, y: Int): Int = x | y"
    run(input)
  }

  test("Operator.Binary.Bitwise ^") {
    val input = "def f(x: Int, y: Int): Int = x ^ y"
    run(input)
  }

  test("Operator.Binary.Bitwise <<") {
    val input = "def f(x: Int, y: Int): Int = x << y"
    run(input)
  }

  test("Operator.Binary.Bitwise >>") {
    val input = "def f(x: Int, y: Int): Int = x >> y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp <") {
    val input = "def f(x: Int, y: Int): Bool = x < y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp <=") {
    val input = "def f(x: Int, y: Int): Bool = x <= y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp >") {
    val input = "def f(x: Int, y: Int): Bool = x > y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp >=") {
    val input = "def f(x: Int, y: Int): Bool = x >= y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp ==") {
    val input = "def f(x: Int, y: Int): Bool = x == y"
    run(input)
  }

  test("Operator.Binary.ComparisonOp !=") {
    val input = "def f(x: Int, y: Int): Bool = x != y"
    run(input)
  }

  test("Operator.Binary.MultiplicativeOp *") {
    val input = "def f(x: Int, y: Int): Int = x * y"
    run(input)
  }

  test("Operator.Binary.MultiplicativeOp /") {
    val input = "def f(x: Int, y: Int): Int = x / y"
    run(input)
  }

  test("Operator.Binary.MultiplicativeOp %") {
    val input = "def f(x: Int, y: Int): Int = x % y"
    run(input)
  }

  test("Operator.Binary.MultiplicativeOp **") {
    val input = "def f(x: Int, y: Int): Int = x ** y"
    run(input)
  }

  test("Operator.Binary.AdditiveOp +") {
    val input = "def f(x: Int, y: Int): Int = x + y"
    run(input)
  }

  test("Operator.Binary.AdditiveOp -") {
    val input = "def f(x: Int, y: Int): Int = x - y"
    run(input)
  }

  test("Operator.Binary.Math.⊥") {
    val input = "def ⊥: Int = ⊥()"
    run(input)
  }

  test("Operator.Binary.Math.⊤") {
    val input = "def ⊤: Int = ⊤()"
    run(input)
  }

  test("Operator.Binary.Math.⊑") {
    val input = "def ⊑(x: Int, y: Int): Int = x ⊑ y"
    run(input)
  }

  test("Operator.Binary.Math.⊔") {
    val input = "def ⊔(x: Int, y: Int): Int = x ⊔ y"
    run(input)
  }

  test("Operator.Binary.Math.⊓") {
    val input = "def ⊓(x: Int, y: Int): Int = x ⊓ y"
    run(input)
  }

  test("Operator.Binary.Math.∇") {
    val input = "def ∇(x: Int, y: Int): Int = x ∇ y"
    run(input)
  }

  test("Operator.Binary.Math.∆") {
    val input = "def ∆(x: Int, y: Int): Int = x ∆ y"
    run(input)
  }

  test("Operator.Binary.Math.⊡") {
    val input = "def ⊡(x: Int, y: Int): Int = x ⊡ y"
    run(input)
  }

  test("Operator.Binary.Math.∈") {
    val input = "def ∈(x: Int, y: Int): Int = x ∈ y"
    run(input)
  }

  test("Operator.Binary.Math.⊕") {
    val input = "def ⊕(x: Int, y: Int): Int = x ⊕ y"
    run(input)
  }

  test("Operator.Binary.Math.⊗") {
    val input = "def ⊗(x: Int, y: Int): Int = x ⊗ y"
    run(input)
  }

  test("Operator.Binary.Math.↪") {
    val input = "def ↪(x: Int, y: Int): Int = x ↪ y"
    run(input)
  }

  test("Operator.Binary.Math.↝") {
    val input = "def ↝(x: Int, y: Int): Int = x ↝ y"
    run(input)
  }

  test("Operator.Binary.++") {
    val input = "def ++(x: Int, y: Int): Int = x ++ y"
    run(input)
  }

  test("Operator.Binary.--") {
    val input = "def --(x: Int, y: Int): Int = x -- y"
    run(input)
  }

  test("Operator.Binary.|>") {
    val input = "def |>(x: Int, y: Int): Int = x |> y"
    run(input)
  }

  test("Operator.Binary.<|") {
    val input = "def <|(x: Int, y: Int): Int = x <| y"
    run(input)
  }

  test("Operator.Binary.<*>") {
    val input = "def <*>(x: Int, y: Int): Int = x <*> y"
    run(input)
  }

  test("Operator.Binary.<**>") {
    val input = "def <**>(x: Int, y: Int): Int = x <**> y"
    run(input)
  }

  test("Operator.Binary.*>") {
    val input = "def *>(x: Int, y: Int): Int = x *> y"
    run(input)
  }

  test("Operator.Binary.<*") {
    val input = "def <*(x: Int, y: Int): Int = x <* y"
    run(input)
  }

  test("Operator.Binary.|+|") {
    val input = "def |+|(x: Int, y: Int): Int = x |+| y"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Annotations                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Annotation.@associative") {
    val input =
      """@associative
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@commutative") {
    val input =
      """@commutative
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@internal") {
    val input =
      """@internal
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@monotone") {
    val input =
      """@monotone @unchecked
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@strict") {
    val input =
      """@strict @unchecked
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@unchecked") {
    val input =
      """@unchecked
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@unsafe") {
    val input =
      """@unsafe
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  test("Annotation.@strict @monotone @unchecked") {
    val input =
      """@strict @monotone @unchecked
        |def f(x: Int, y: Int): Int = 21
      """.stripMargin
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("WhiteSpace.01") {
    val input = ""
    run(input)
  }

  test("WhiteSpace.02") {
    val input = " "
    run(input)
  }

  test("WhiteSpace.03") {
    val input = "    "
    run(input)
  }

  test("WhiteSpace.04") {
    val input = "\t"
    run(input)
  }

  test("WhiteSpace.NewLine.01") {
    val input = "\n"
    run(input)
  }

  test("WhiteSpace.NewLine.02") {
    val input = "\r"
    run(input)
  }

  test("WhiteSpace.NewLine.03") {
    val input = "\r\n"
    run(input)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("SingleLineComment.01") {
    val input = "// a comment"
    run(input)
  }

  test("SingleLineComment.02") {
    val input =
      """// a comment
        |// another comment
        |// and yet another
      """.stripMargin
    run(input)
  }

  test("SingleLineComment.03") {
    val input =
      """//////////////////////////////////////////////////////////////////////
        |//////////////////////////////////////////////////////////////////////
        |//////////////////////////////////////////////////////////////////////
      """.stripMargin
    run(input)
  }

  test("MultiLineComment.01") {
    val input = "/* a comment */"
    run(input)
  }

  test("MultiLineComment.02") {
    val input =
      """/*
        |a comment
        |*/
      """.stripMargin
    run(input)
  }

  test("TripleSlashComment.01") {
    val input = "/// a comment"
    run(input)
  }

  test("Comment.01") {
    val input =
      """
        |
        |   /* hello */ def
        |   /* world */
        |   foo(/* a nice arg */ a: Int): /* lets return something */ Bool = true
        |
        |
      """.stripMargin
    run(input)
  }

  test("Comment.02") {
    val input =
      """
        |
        |   def f: Bool =
        |     if (/* oh a comment */ true) /* another */ true else
        |     // now what?
        |     false
        |
        |
      """.stripMargin
    run(input)
  }

}
