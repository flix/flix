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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.Resolver.ResolverError
import org.scalatest.FunSuite

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("Root.01") {
    val input = ""
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Imports                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Import.Wildcard.01") {
    val input1 =
      """namespace a.b.c {
        |  def f: Int = 42
        |}
      """.stripMargin
    val input2 =
      """import a.b.c/_
        |def g: Int = f() + 42
      """.stripMargin
    new Flix().addStr(input1).addStr(input2).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Import.Definition.01") {
    val input1 =
      """namespace a.b.c {
        |  def f: Int = 42
        |}
      """.stripMargin
    val input2 =
      """import a.b.c/f
        |def g: Int = f() + 42
      """.stripMargin
    new Flix().addStr(input1).addStr(input2).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Import.Namespace.01") {
    val input1 =
      """namespace a.b.c {
        |  def f: Int = 42
        |}
      """.stripMargin
    val input2 =
      """import a.b.c
        |def g: Int = c/f() + 42
      """.stripMargin
    new Flix().addStr(input1).addStr(input2).compile().errors.head.isInstanceOf[ResolverError]
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
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Namespace.02") {
    val input =
      """namespace a.b.c {
        |  // comment
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Definition.01") {
    val input = "def f: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Definition.02") {
    val input = "def f(x: Int): Int = x + 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Definition.03") {
    val input = "def f(x: Int, y: Int, z: Int): Int = x + y + z + 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum0.1") {
    val input =
      """enum A {
        |  case B
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum.02") {
    val input =
      """enum A {
        |  case B(Int)
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum.03") {
    val input =
      """enum A {
        |  case B,
        |  case C(Int),
        |  case D(Bool, Int, Str)
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation.01") {
    val input = "rel R(a: Int)"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation.02") {
    val input = "rel R(a: Char, b: Int, c: Str)"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation.03") {
    val input = "rel R(a: Int8, b: Int16, c: Int32, d: Int64)"
    new Flix().addStr(input).compile().get
  }

  test("Lattice.01") {
    val input = "lat L(a: A)"
    new Flix().addStr(input).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Lattice.02") {
    val input = "lat L(a: A, b: B, c: C)"
    new Flix().addStr(input).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Declaration.Index.01") {
    val input =
      """rel R(a: Int)
        |index R({a});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Index.02") {
    val input =
      """rel R(a: Char, b: Int)
        |index R({a}, {b});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Index.03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |index R({a}, {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}, {a, b, c}, {a, b, c, d});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Class.01") {
    val input =
      """class Eq[A] {
        |  def eq(x: A, y: B): Bool
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Class.02") {
    val input =
      """class Coerce[A, B] {
        |  def coerce(a: A): B
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
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
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Class.04") {
    val input =
      """class Eq[A] => PartialOrd[A], PreOrd[A] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law.01") {
    val input = "law f: Bool = true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law.02") {
    val input = "law f(x: Int): Bool = x % 2 == 0"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law.03") {
    val input = "law f(x: Int, y: Int): Bool = x > y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl.01") {
    val input =
      """impl Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl.02") {
    val input =
      """impl Eq[(Int, Int)] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl.03") {
    val input =
      """impl Ord[Int] <= Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl.04") {
    val input =
      """impl A[Int, Int] <= B[Int], C[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unit.01") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Char.01") {
    val input = "def f: Char = 'a'"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Char.02") {
    val input = "def f: Char = 'x'"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Float32.01") {
    val input = "def f: Float32 = 123.456f32"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Float32.02") {
    val input = "def f: Float32 = -123.456f32"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Float64.01") {
    val input = "def f: Float64 = 123.456f64"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Float64.02") {
    val input = "def f: Float64 = -123.456f64"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int8.01") {
    val input = "def f: Int8 = 123i8"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int16.01") {
    val input = "def f: Int16 = 123i16"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int32.01") {
    val input = "def f: Int32 = 123i32"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int64.01") {
    val input = "def f: Int64 = 123i64"
    new Flix().addStr(input).compile().get
  }

  test("Expression.BigInt.01") {
    val input = "def f: BigInt = 123ii"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp.01") {
    val input = "def f: Bool = true && false"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp.02") {
    val input = "def f: Bool = true || false"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp.03") {
    val input = "def f: Bool = 1 < 2 && 3 < 4"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp.01") {
    val input = "def f: Bool = 1 < 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp.02") {
    val input = "def f: Bool = 1 + 2 > 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp.03") {
    val input = "def f: Bool = 1 + 2 > 3 + 4"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp.01") {
    val input = "def f: Int = 1 * 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp.02") {
    val input = "def f: Int = 1 * 2 * 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp.03") {
    val input = "def f: Int = 1 * 2 + 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp.04") {
    val input = "def f: Int = 1 + 2 * 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp.01") {
    val input = "def f: Int = 1 + 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp.02") {
    val input = "def f: Int = 1 + 2 + 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp.03") {
    val input = "def f: Int = 1 - 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp.04") {
    val input = "def f: Int = 1 - 2 - 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp.05") {
    val input = "def f: Int = 1 + 2 - 3 + 4 - 5 + 6"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Infix.01") {
    val input =
      """def plus(x: Int, y: Int): Int =  x + y
        |
        |def f: Int = 1 `plus` 2
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Infix.02") {
    val input =
      """namespace a.b.c {
        |  def plus(x: Int, y: Int): Int =  x + y
        |}
        |
        |def f: Int = 1 `a.b.c/plus` 2
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp.01") {
    val input = "def f: Int = +1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp.02") {
    val input = "def f: Int = -1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp.03") {
    val input = "def f: Int = ~1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp.04") {
    val input = "def f: Bool = !!true"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Ascribe.01") {
    val input = "def f: Bool = true: Bool"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch.01") {
    val input = "def f: Int = let x = 42 in x"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch.02") {
    val input = "def f: Int = let (x, y) = (42, 21) in x + y"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch.03") {
    val input =
      """def f: Int =
        |  let x = 1 in
        |  let y = 2 in
        |  let z = 3 in
        |    42
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch.04") {
    // Note: This is to test the performance of deeply nested lets.
    val input =
    """
      |def f: Int =
      |    let x1 = 1 in
      |    let x2 = 1 in
      |    let x3 = 1 in
      |    let x4 = 1 in
      |    let x5 = 1 in
      |    let x6 = 1 in
      |    let x7 = 1 in
      |    let x8 = 1 in
      |    let x9 = 1 in
      |    let y1 = 1 in
      |    let y2 = 1 in
      |    let y3 = 1 in
      |    let y4 = 1 in
      |    let y5 = 1 in
      |    let y6 = 1 in
      |    let y7 = 1 in
      |    let y8 = 1 in
      |    let y9 = 1 in
      |    let z1 = 1 in
      |    let z2 = 1 in
      |    let z3 = 1 in
      |    let z4 = 1 in
      |    let z5 = 1 in
      |    let z6 = 1 in
      |    let z7 = 1 in
      |    let z8 = 1 in
      |    let z9 = 1 in
      |        1
    """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElse.01") {
    val input = "def f: Int = if (true) 42 else 21"
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElse.02") {
    val input = "def f: Int = if ((true)) (1) else (2)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElse.03") {
    val input = "def f: (Int, Int) = if (true || false) (1, 2) else (3, 4)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch.01") {
    val input =
      """def f(x: Int): Int = switch {
        |  case true  => 1
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch.02") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch.03") {
    val input =
      """def f(x: Int): Int = switch {
        |  case x < 0  => 1
        |  case x > 0  => 2
        |  case x == 0 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Match.01") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Match.02") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply.01") {
    val input =
      """def f: Int = 42
        |def g: Int = f()
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply.02") {
    val input =
      """def f(x: Int): Int = x
        |def g: Int = f(42)
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply.03") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, 2)
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply.04") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, f(2, 3))
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply.05") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(f(1, 2), f(3, 4))
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Enum.01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Green,
        |  case Blue
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Enum.02") {
    val input =
      """enum Shape {
        |  case Circle(Int),
        |  case Square(Int, Int)
        |}
        |
        |def f: Shape = Shape.Circle(42)
        |def g: Shape = Shape.Square(21, 42)
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple.01") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple.02") {
    val input = "def f: Int = (42)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple.03") {
    val input = "def f: (Int, Int) = (42, 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple.04") {
    val input = "def f(x: Int): (Int, Int, Int) = (42, x, 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple.05") {
    val input = "def f(x: Int): (Int, (Int, Int), Int) = (42, (x, x), 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Opt.01") {
    val input = "def f: Opt[Char] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt.02") {
    val input = "def f: Opt[Int] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt.03") {
    val input = "def f: Opt[Char] = Some('a')"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt.04") {
    val input = "def f: Opt[Int] = Some(42)"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt.05") {
    val input = "def f: Opt[(Char, Int)] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt.06") {
    val input = "def f: Opt[(Char, Int)] = Some(('a', 42))"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List.01") {
    val input = "def f: List[Int] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List.02") {
    val input = "def f: List[Int] = 1 :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List.03") {
    val input = "def f: List[Int] = 1 :: 2 :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List.04") {
    val input = "def f: List[(Int, Int)] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List.05") {
    val input = "def f: List[(Int, Int)] = (1, 2) :: (3, 4) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList.01") {
    val input = "def f: List[List[Int]] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList.02") {
    val input = "def f: List[List[Int]] = (1 :: Nil) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList.03") {
    val input = "def f: List[List[Int]] = (Nil) :: (1 :: Nil) :: (2 :: 3 :: 4 :: Nil) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.01") {
    val input = "def f: Vec[Int] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.02") {
    val input = "def f: Vec[Int] = #[1]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.03") {
    val input = "def f: Vec[Int] = #[1, 2]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.04") {
    val input = "def f: Vec[Int] = #[1, 2, 3]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.05") {
    val input = "def f: Vec[(Char, Int)] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec.06") {
    val input = "def f: Vec[(Char, Int)] = #[('a', 21), ('b', 42)]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.VecVec.01") {
    val input = "def f: Vec[Vec[Int]] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.VecVec.03") {
    val input = "def f: Vec[Vec[Int]] = #[#[], #[1], #[1, 2, 3]]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Set.01") {
    // TODO: Pending new type system.
    intercept[AssertionError] {
      val input = "def f: Set[Int] = #{}"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Set.02") {
    val input = "def f: Set[Int] = #{1}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set.03") {
    val input = "def f: Set[Int] = #{1, 2}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set.04") {
    val input = "def f: Set[Int] = #{1, 2, 3}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set.05") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2)}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set.06") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2), (3, 4)}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set.07") {
    val input = "def f: Set[Int] = #{1 + 2, 3 + 4, 5 + 6}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.SetSet.01") {
    // TODO: Pending new type system.
    intercept[AssertionError] {
      val input = "def f: Set[Set[Int]] = #{}"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.SetSet.02") {
    // TODO: Pending new type system.
    intercept[AssertionError] {
      val input = "def f: Set[Set[Int]] = #{#{}}"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.SetSet.03") {
    val input = "def f: Set[Set[Int]] = #{#{1, 2}, #{3, 4}, #{5, 6}}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Map.01") {
    val input = "def f: Map[Char, Int] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map.02") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map.03") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map.04") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2, 'c' -> 3}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map.05") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map.06") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{(1i8, 2i16) -> (3i32, 4i64)}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap.01") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap.02") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap.03") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}, 2 -> @{3 -> 'a', 4 -> 'b'}}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapList.01") {
    val input = "def f: Map[Int, List[Int]] = @{1 -> 2 :: 3 :: Nil, 4 -> 5 :: 6 :: Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet.01") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet.02") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet.04") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> #{1, 2, 3} :: #{4, 5, 6} :: Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Var.01") {
    val input = "def f(x: Int): Int = x"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Lambda.01") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Int -> Int = x -> x"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.02") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Int -> Int = (x) -> x"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.03") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Char) -> Int = (x, y) -> 42"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.04") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Char, Int) -> Int = (x, y, z) -> 42"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.05") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Int8, Int16, Int32, Int64) -> Int32 = (x, y, z, w) -> z"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.06") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Int -> (Bool, Char) = x -> (true, 'a')"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.07") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Int -> (Bool, Char, Int) = x -> (true, 'a', 42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.08") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Char) -> (Char, Bool) = (x, y) -> (y, x)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.09") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Char, Int) -> (Int, Char, Bool) = (x, y, z) -> (z, y, x)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.10") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: ((Bool, Char), Int) -> (Bool, Char) = (x, y) -> x"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.11") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, (Char, Int)) -> (Char, Int) = (x, y) -> y"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.12") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Int, Int) -> ((Int, Int), (Int, Int)) = x -> (x, x)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.13") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Bool -> Char -> Int = x -> (y -> 42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.14") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Bool) -> Char -> Int = (x1, x2) -> (y -> 42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.15") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Bool -> (Char, Char) -> Int = x -> ((y1, y2) -> 42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.16") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: Bool -> Char -> (Int, Int) = x -> (y -> (21, 42))"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Lambda.17") {
    // TODO: Pending new type system.
    intercept[NotImplementedError] {
      val input = "def f: (Bool, Bool) -> (Char, Char) -> (Int, Int) = (x1, x2) -> ((y1, y2) -> (21, 42))"
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Bot.01") {
    val input = "def ⊥: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Top.01") {
    val input = "def ⊤: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Leq.01") {
    val input = "def ⊑: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Lub.01") {
    val input = "def ⊔: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Glb.01") {
    val input = "def ⊓: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Existential.01") {
    val input = "def f: Prop = ∃(x: Bool). true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Existential.02") {
    val input = "def f: Prop = ∃(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Existential.03") {
    val input = "def f: Prop = \\exists(x: Bool). true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Existential.04") {
    val input = "def f: Prop = \\exists(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Universal.01") {
    val input = "def f: Prop = ∀(x: Bool). true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Universal.02") {
    val input = "def f: Prop = ∀(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Universal.03") {
    val input = "def f: Prop = \\forall(x: Bool). true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Universal.04") {
    val input = "def f: Prop = \\forall(x: Int, y: Int). x == y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Var.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case x => x
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Unit.01") {
    val input =
      """def f(x: Unit): Int = match x with {
        |  case () => 42
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Bool.01") {
    val input =
      """def f(x: Bool): Int = match x with {
        |  case true => 42
        |  case false => 21
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Char.01") {
    val input =
      """def f(x: Char): Int = match x with {
        |  case 'a' => 1
        |  case 'b' => 2
        |  case 'c' => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Int.01") {
    val input =
      """def f(x: Int): Int = match x with {
        |  case 1 => 1
        |  case 2 => 2
        |  case 3 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Int.02") {
    val input =
      """def f(x: Int16): Int = match x with {
        |  case 1i16 => 1
        |  case 2i16 => 2
        |  case 3i16 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Literal.Str.01") {
    val input =
      """def f(x: Str): Int = match x with {
        |  case "foo" => 1
        |  case "bar" => 2
        |  case "baz" => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Enum.01") {
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
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Enum.02") {
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
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Tuple.01") {
    val input =
      """def f(x: (Bool, Char, Int)): Int = match x with {
        |  case (true, 'a', 42) => 1
        |  case (false, 'b', 21) => 2
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Pattern.Opt.01") {
    val input =
      """def f(o: Opt[Int]): Int = match o with {
        |  case None => 0
        |  case Some(x) => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Opt.02") {
    val input =
      """def f(o: Opt[Int]): Int = match o with {
        |  case None => 0
        |  case Some(1) => 1
        |  case Some(2) => 2
        |  case Some(x) => x + x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Opt.03") {
    val input =
      """def f(o: Opt[Char]): Int = match o with {
        |  case None => 0
        |  case Some('a') => 1
        |  case Some('b') => 2
        |  case Some(c)   => 3
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.01") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.02") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.03") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.04") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: 3 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.05") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: Nil => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.06") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: y :: Nil => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.07") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |  case x :: rs => 1 + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.08") {
    val input =
      """def f(xs: List[Int]): Bool = match xs with {
        |  case Nil => true
        |  case x :: y :: rs => f(rs)
        |  case _ => false
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
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
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.10") {
    val input =
      """def f(xs: List[(Char, Int)]): Int = match xs with {
        |  case Nil => 0
        |  case (c, i) :: Nil => i
        |  case (c1, i1) :: (c2, i2) :: Nil => i1 + i2
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List.11") {
    val input =
      """def f(xs: List[(Char, Int)]): Int = match xs with {
        |  case Nil => 0
        |  case (c, 42) :: Nil => 1
        |  case ('a', i1) :: (c2, 21) :: Nil => 2
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.ListList.01") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: Nil) :: (y :: Nil) :: Nil => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.ListList.02") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: y :: Nil) :: (z :: w :: Nil) :: Nil => x + y + z + w
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.ListList.03") {
    val input =
      """def f(xs: List[List[Int]]): Int = match xs with {
        |  case Nil => 0
        |  case (x :: xs) :: (y :: ys) :: (z :: zs) :: Nil => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.01") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.02") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.03") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.04") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2, 3] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.05") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x] => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.06") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y] => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.07") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z] => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec.08") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z, rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec.01") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec.02") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[1, 2, 3], #[4, 5, 6]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec.03") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[], #[1], #[1, 2, 3], #[x, y, z], rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.01") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.02") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.03") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.04") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3, rs...} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.05") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.06") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set.07") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y, z} => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
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
      new Flix().addStr(input).compile().get
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
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.SetSet.01") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.SetSet.02") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x}, #{y}, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.SetSet.03") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x, y, as...}, #{z, w, bs...}, rs...} => x + y + z + w
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.01") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.02") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.03") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.04") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21, c -> 11} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.05") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.06") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map.07") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
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
      new Flix().addStr(input).compile().get
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Facts and Rules                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("Declaration.Fact.01") {
    val input =
      """rel R(a: Int)
        |R(42).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact.02") {
    val input =
      """rel R(a: Char, b: Int)
        |R('a', 42).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact.03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |R(1i8, 2i16, 3i32, 4i64).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.01") {
    val input =
      """rel R(a: Int)
        |
        |R(x) :- R(x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.02") {
    val input =
      """rel R(a: Int)
        |
        |R(x) :- R(x), R(x), R(x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.03") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |R(x, y) :- R(x, y), R(y, x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact.Head.True") {
    val input = "true."
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact.Head.False") {
    val input = "false."
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.Head.True") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |true :- R(x, y).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.Head.False") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |false :- R(x, y).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule.04") {
    val input =
      """def f: Int = 42
        |
        |rel R(a: Int)
        |
        |R(f()).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Unit.01") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Type.Bool.01") {
    val input = "def f: Bool = true"
    new Flix().addStr(input).compile().get
  }

  test("Type.Bool.02") {
    val input = "def f: Bool = false"
    new Flix().addStr(input).compile().get
  }

  test("Type.Char.01") {
    val input = "def f: Char = 'a'"
    new Flix().addStr(input).compile().get
  }

  test("Type.Float32.01") {
    val input = "def f: Float32 = 0.0f32"
    new Flix().addStr(input).compile().get
  }

  test("Type.Float64.01") {
    val input = "def f: Float64 = 0.0f64"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int8.01") {
    val input = "def f: Int8 = 0i8"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int16.01") {
    val input = "def f: Int16 = 0i16"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int32.01") {
    val input = "def f: Int32 = 0i32"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int64.01") {
    val input = "def f: Int64 = 0i64"
    new Flix().addStr(input).compile().get
  }

  test("Type.BigInt.01") {
    val input = "def f: BigInt = 0ii"
    new Flix().addStr(input).compile().get
  }

  test("Type.Str.01") {
    val input = "def f: Str = \"foobar\""
    new Flix().addStr(input).compile().get
  }

  test("Type.Enum.01") {
    val input =
      """enum Color {
        |  case Red
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Type.Tuple.01") {
    val input = "def f: (Int, Int) = (1, 2)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Tuple.02") {
    val input = "def f: (Unit, Bool, Char, Int) = ((), true, 'a', 42)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Lambda.01") {
    intercept[NotImplementedError] {
      val input = "def f: Int = (x -> x + 1)(42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.Lambda.02") {
    intercept[NotImplementedError] {
      val input = "def f: Int = ((x, y) -> x + y)(21, 42)"
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.Opt.01") {
    val input = "def f: Opt[Int] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.List.01") {
    val input = "def f: List[Int] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.Vec.01") {
    val input = "def f: Vec[Int] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.Set.01") {
    intercept[AssertionError] {
      val input = "def f: Set[Int] = #{}"
      new Flix().addStr(input).compile().get
    }
  }

  test("Type.Map.01") {
    val input = "def f: Map[Int, Int] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Ident.01") {
    val input = "def x: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.02") {
    val input = "def xx: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.03") {
    val input = "def xxx: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.04") {
    val input = "def xY: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.05") {
    val input = "def xxxYyy: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.06") {
    val input = "def xxxYyyZzz: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.07") {
    val input = "def x0: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.08") {
    val input = "def x0123: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.09") {
    val input = "def x_y_z: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident.10") {
    val input = "def x_Y32Y_15zz: Int = 42"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Literal.Unit.01") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Literal.True.01") {
    val input = "def f: Bool = true"
    new Flix().addStr(input).compile().get
  }

  test("Literal.False.01") {
    val input = "def f: Bool = false"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Char.01") {
    val input = "def f: Char = 'a'"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float32.01") {
    val input = "def f: Float32 = 123.456f32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float32.02") {
    val input = "def f: Float32 = +123.456f32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float32.03") {
    val input = "def f: Float32 = -123.456f32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float64.01") {
    val input = "def f: Float64 = 123.456f64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float64.02") {
    val input = "def f: Float64 = +123.456f64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Float64.03") {
    val input = "def f: Float64 = -123.456f64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int8.01") {
    val input = "def f: Int8 = 123i8"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int8.02") {
    val input = "def f: Int8 = +123i8"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int8.03") {
    val input = "def f: Int8 = -123i8"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int16.01") {
    val input = "def f: Int16 = 123i16"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int16.02") {
    val input = "def f: Int16 = +123i16"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int16.03") {
    val input = "def f: Int16 = -123i16"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int32.01") {
    val input = "def f: Int32 = 123i32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int32.02") {
    val input = "def f: Int32 = +123i32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int32.03") {
    val input = "def f: Int32 = -123i32"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int64.01") {
    val input = "def f: Int64 = 123i64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int64.02") {
    val input = "def f: Int64 = +123i64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Int64.03") {
    val input = "def f: Int64 = -123i64"
    new Flix().addStr(input).compile().get
  }

  test("Literal.BigInt.01") {
    val input = "def f: BigInt = 123ii"
    new Flix().addStr(input).compile().get
  }

  test("Literal.BigInt.02") {
    val input = "def f: BigInt = +123ii"
    new Flix().addStr(input).compile().get
  }

  test("Literal.BigInt.03") {
    val input = "def f: BigInt = -123ii"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Operators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  test("Operator.Unary !") {
    val input = "def f(b: Bool): Bool = !b"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Unary +") {
    val input = "def f(i: Int): Int = +i"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Unary -") {
    val input = "def f(i: Int): Int = -i"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Unary ~") {
    val input = "def f(i: Int): Int = ~i"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.LogicalOp &&") {
    val input = "def f(x: Bool, y: Bool): Bool = x && y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.LogicalOp ||") {
    val input = "def f(x: Bool, y: Bool): Bool = x || y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.LogicalOp ==>") {
    val input = "def f(x: Bool, y: Bool): Bool = x ==> y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.LogicalOp <==>") {
    val input = "def f(x: Bool, y: Bool): Bool = x <==> y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.Bitwise &") {
    val input = "def f(x: Int, y: Int): Int = x & y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.Bitwise |") {
    val input = "def f(x: Int, y: Int): Int = x | y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.Bitwise ^") {
    val input = "def f(x: Int, y: Int): Int = x ^ y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.Bitwise <<") {
    val input = "def f(x: Int, y: Int): Int = x << y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.Bitwise >>") {
    val input = "def f(x: Int, y: Int): Int = x >> y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp <") {
    val input = "def f(x: Int, y: Int): Bool = x < y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp <=") {
    val input = "def f(x: Int, y: Int): Bool = x <= y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp >") {
    val input = "def f(x: Int, y: Int): Bool = x > y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp >=") {
    val input = "def f(x: Int, y: Int): Bool = x >= y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp ==") {
    val input = "def f(x: Int, y: Int): Bool = x == y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.ComparisonOp !=") {
    val input = "def f(x: Int, y: Int): Bool = x != y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.MultiplicativeOp *") {
    val input = "def f(x: Int, y: Int): Int = x * y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.MultiplicativeOp /") {
    val input = "def f(x: Int, y: Int): Int = x / y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.MultiplicativeOp %") {
    val input = "def f(x: Int, y: Int): Int = x % y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.MultiplicativeOp **") {
    val input = "def f(x: Int, y: Int): Int = x ** y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.AdditiveOp +") {
    val input = "def f(x: Int, y: Int): Int = x + y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.AdditiveOp -") {
    val input = "def f(x: Int, y: Int): Int = x - y"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // UTF8 Operators                                                          //
  /////////////////////////////////////////////////////////////////////////////
  test("Operator.Unary.UTF8-Negation") {
    val input = "def f(b: Bool): Bool = ¬b"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.UTF8-Equal") {
    val input = "def f(x: Int, y: Int): Bool = x ≡ y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.UTF8-Conjunction") {
    val input = "def f(x: Bool, y: Bool): Bool = x ∧ y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.UTF8-Disjunction") {
    val input = "def f(x: Bool, y: Bool): Bool = x ∨ y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.UTF8-Implication") {
    val input = "def f(x: Bool, y: Bool): Bool = x → y"
    new Flix().addStr(input).compile().get
  }

  test("Operator.Binary.UTF8-Biconditional") {
    val input = "def f(x: Bool, y: Bool): Bool = x ↔ y"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Annotations                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Annotation.01") {
    val input =
      """@strict
        |def f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation.02") {
    val input =
      """@monotone
        |def f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation.03") {
    val input =
      """@strict @monotone
        |def f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation.04") {
    val input =
      """@strict @monotone @commutative @associative @unsafe @unchecked
        |def f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("WhiteSpace.01") {
    val input = " "
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace.02") {
    val input = "    "
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace.03") {
    val input = "\t"
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace.NewLine.Unix") {
    val input = "\n"
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace.NewLine.Windows") {
    val input = "\r\n"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("SingleLineComment.01") {
    val input = "// a comment"
    new Flix().addStr(input).compile().get
  }

  test("SingleLineComment.02") {
    val input =
      """// a comment
        |// another comment
        |// and yet another
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("MultiLineComment.01") {
    val input = "/* a comment */"
    new Flix().addStr(input).compile().get
  }

  test("MultiLineComment.02") {
    val input =
      """/*
        |a comment
        |*/
      """.stripMargin
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
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
    new Flix().addStr(input).compile().get
  }

}
