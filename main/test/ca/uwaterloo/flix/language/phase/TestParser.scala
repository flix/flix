package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{ParsedAst, _}
import ca.uwaterloo.flix.language.phase.Resolver.ResolverError
import org.scalatest.FunSuite

class TestParser extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Root                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("Root01") {
    val input = ""
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Imports                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Import.Wildcard01") {
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

  test("Import.Definition01") {
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

  test("Import.Namespace01") {
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
  test("Declaration.Namespace01") {
    val input =
      """namespace a {
        |  // comment
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Namespace02") {
    val input =
      """namespace a.b.c {
        |  // comment
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Namespace03") {
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

  test("Declaration.Namespace04") {
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

  test("Declaration.Namespace05") {
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

  test("Declaration.Namespace06") {
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

  test("Declaration.Namespace07") {
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

  test("Declaration.Namespace08") {
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

  test("Declaration.Namespace09") {
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

  test("Declaration.Function01") {
    val input = "def f: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Function02") {
    val input = "def f(x: Int): Int = x + 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Function03") {
    val input = "def f(x: Int, y: Int, z: Int): Int = x + y + z + 42"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum01") {
    val input =
      """enum A {
        |  case B
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum02") {
    val input =
      """enum A {
        |  case B(Int)
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Enum03") {
    val input =
      """enum A {
        |  case B,
        |  case C(Int),
        |  case D(Bool, Int, Str)
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation01") {
    val input = "rel R(a: Int)"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation02") {
    val input = "rel R(a: Char, b: Int, c: Str)"
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Relation03") {
    val input = "rel R(a: Int8, b: Int16, c: Int32, d: Int64)"
    new Flix().addStr(input).compile().get
  }

  test("Lattice01") {
    val input = "lat L(a: A)"
    new Flix().addStr(input).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Lattice02") {
    val input = "lat L(a: A, b: B, c: C)"
    new Flix().addStr(input).compile().errors.head.isInstanceOf[ResolverError]
  }

  test("Declaration.Index01") {
    val input =
      """rel R(a: Int)
        |index R({a});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Index02") {
    val input =
      """rel R(a: Char, b: Int)
        |index R({a}, {b});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Index03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |index R({a}, {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}, {a, b, c}, {a, b, c, d});
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Class01") {
    val input =
      """class Eq[A] {
        |  def eq(x: A, y: B): Bool
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Class02") {
    val input =
      """class Coerce[A, B] {
        |  def coerce(a: A): B
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Class03") {
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

  test("Declaration.Class04") {
    val input =
      """class Eq[A] => PartialOrd[A], PreOrd[A] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law01") {
    val input = "law f(): Bool = true"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law02") {
    val input = "law f(x: Int): Bool = x % 2 == 0"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Law03") {
    val input = "law f(x: Int, y: Int): Bool = x > y"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl01") {
    val input =
      """impl Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl02") {
    val input =
      """impl Eq[(Int, Int)] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl03") {
    val input =
      """impl Ord[Int] <= Eq[Int] {
        |  /* ... */
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Declaration.Impl04") {
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

  test("Expression.Char01") {
    val input = "def f: Char = 'a'"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Char02") {
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

  test("Expression.Int8") {
    val input = "def f: Int8 = 123i8"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int16") {
    val input = "def f: Int16 = 123i16"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int32") {
    val input = "def f: Int32 = 123i32"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Int64") {
    val input = "def f: Int64 = 123i64"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp01") {
    val input = "def f: Bool = true && false"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp02") {
    val input = "def f: Bool = true || false"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LogicalExp03") {
    val input = "def f: Bool = 1 < 2 && 3 < 4"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp01") {
    val input = "def f: Bool = 1 < 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp02") {
    val input = "def f: Bool = 1 + 2 > 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.ComparisonExp03") {
    val input = "def f: Bool = 1 + 2 > 3 + 4"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp01") {
    val input = "def f: Int = 1 * 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp02") {
    val input = "def f: Int = 1 * 2 * 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp03") {
    val input = "def f: Int = 1 * 2 + 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.MultiplicativeExp04") {
    val input = "def f: Int = 1 + 2 * 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp01") {
    val input = "def f: Int = 1 + 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp02") {
    val input = "def f: Int = 1 + 2 + 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp03") {
    val input = "def f: Int = 1 - 2"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp04") {
    val input = "def f: Int = 1 - 2 - 3"
    new Flix().addStr(input).compile().get
  }

  test("Expression.AdditiveExp05") {
    val input = "def f: Int = 1 + 2 - 3 + 4 - 5 + 6"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Infix01") {
    val input =
      """def plus(x: Int, y: Int): Int =  x + y
        |
        |def f: Int = 1 `plus` 2
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Infix02") {
    val input =
      """namespace a.b.c {
        |  def plus(x: Int, y: Int): Int =  x + y
        |}
        |
        |def f: Int = 1 `a.b.c/plus` 2
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp01") {
    val input = "def f: Int = +1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp02") {
    val input = "def f: Int = -1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp03") {
    val input = "def f: Int = ~1"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UnaryExp04") {
    val input = "def f: Bool = !!true"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Ascribe01") {
    val input = "def f: Bool = true: Bool"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch01") {
    val input = "def f: Int = let x = 42 in x"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch02") {
    val input = "def f: Int = let (x, y) = (42, 21) in x + y"
    new Flix().addStr(input).compile().get
  }

  test("Expression.LetMatch03") {
    val input =
      """def f: Int =
        |  let x = 1 in
        |  let y = 2 in
        |  let z = 3 in
        |    42
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElseExp01") {
    val input = "def f: Int = if (true) 42 else 21"
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElseExp02") {
    val input = "def f: Int = if ((true)) (1) else (2)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.IfThenElseExp03") {
    val input = "def f: (Int, Int) = if (true || false) (1, 2) else (3, 4)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch01") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case true  => 1
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch02") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case x < 0  => 1
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Switch03") {
    val input =
      """fn f(x: Int): Int = switch {
        |  case x < 0  => 1
        |  case x > 0  => 2
        |  case x == 0 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.MatchExp01") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.MatchExp02") {
    val input =
      """def f: Int = match 1 with {
        |  case 2 => 3
        |  case 4 => 5
        |}
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.MatchExp03") {
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

  test("Expression.MatchExp04") {
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

  test("Expression.Apply01") {
    val input =
      """def f: Int = 42
        |def g: Int = f()
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply02") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, 2)
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply03") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(1, f(2, 3))
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Apply04") {
    val input =
      """def f(x: Int, y: Int): Int = x + y
        |def g: Int = f(f(1, 2), f(3, 4))
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tag01") {
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

  test("Expression.Tag02") {
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

  test("Expression.Tuple01") {
    val input = "fn f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple02") {
    val input = "fn f: Int = (42)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple03") {
    val input = "fn f: (Int, Int) = (42, 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple04") {
    val input = "fn f(x: Int): (Int, Int, Int) = (42, x, 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Tuple05") {
    val input = "fn f(x: Int): (Int, (Int, Int), Int) = (42, (x, x), 21)"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Opt01") {
    val input = "def f: Opt[Char] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt02") {
    val input = "def f: Opt[Int] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt03") {
    val input = "def f: Opt[Char] = Some('a')"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt04") {
    val input = "def f: Opt[Int] = Some(42)"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt05") {
    val input = "def f: Opt[(Char, Int)] = None"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Opt06") {
    val input = "def f: Opt[(Char, Int)] = Some(('a', 42))"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List01") {
    val input = "def f: List[Int] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List02") {
    val input = "def f: List[Int] = 1 :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List03") {
    val input = "def f: List[Int] = 1 :: 2 :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List04") {
    val input = "def f: List[(Int, Int)] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.List05") {
    val input = "def f: List[(Int, Int)] = (1, 2) :: (3, 4) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList01") {
    val input = "def f: List[List[Int]] = Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList02") {
    val input = "def f: List[List[Int]] = (1 :: Nil) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.ListList03") {
    val input = "def f: List[List[Int]] = (Nil) :: (1 :: Nil) :: (2 :: 3 :: 4 :: Nil) :: Nil"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec01") {
    val input = "def f: Vec[Int] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec02") {
    val input = "def f: Vec[Int] = #[1]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec03") {
    val input = "def f: Vec[Int] = #[1, 2]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec04") {
    val input = "def f: Vec[Int] = #[1, 2, 3]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec05") {
    val input = "def f: Vec[(Char, Int)] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Vec06") {
    val input = "def f: Vec[(Char, Int)] = #[('a', 21), ('b', 42)]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.VecVec01") {
    val input = "def f: Vec[Vec[Int]] = #[]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.VecVec03") {
    val input = "def f: Vec[Vec[Int]] = #[#[], #[1], #[1, 2, 3]]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.GetIndex01") {
    val input = "def f(v: Vec[Int]): Int = v[0]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.GetIndex02") {
    val input = "def f(v: Vec[Int]): Int = v[0] + v[1] + v[2]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.GetIndex03") {
    val input = "def f(v: Vec[Int], x: Int): Int = v[x]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.PutIndex01") {
    val input = "def f(v: Vec[Int]): Int = v[0 -> 42]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.PutIndex02") {
    val input = "def f(v: Vec[Int]): Int = v[0 -> 21][1 -> 42]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.PutIndex03") {
    val input = "def f(v: Vec[Int], x: Int): Int = v[x -> 0, (x + 1) -> 1]"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  ignore("Expression.Set01") {
    val input = "def f: Set[Int] = #{}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set02") {
    val input = "def f: Set[Int] = #{1}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set03") {
    val input = "def f: Set[Int] = #{1, 2}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set04") {
    val input = "def f: Set[Int] = #{1, 2, 3}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set05") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2)}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set06") {
    val input = "def f: Set[(Int, Int)] = #{(1, 2), (3, 4)}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Set07") {
    val input = "def f: Set[Int] = #{1 + 2, 3 + 4, 5 + 6}"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.SetSet01") {
    val input = "def f: Set[Set[Int]] = #{}"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.SetSet02") {
    val input = "def f: Set[Set[Int]] = #{#{}}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.SetSet03") {
    val input = "def f: Set[Set[Int]] = #{#{1, 2}, #{3, 4}, #{5, 6}}"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Map01") {
    val input = "def f: Map[Char, Int] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map02") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map03") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map04") {
    val input = "def f: Map[Char, Int] = @{'a' -> 1, 'b' -> 2, 'c' -> 3}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map05") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Map06") {
    val input = "def f: Map[(Int8, Int16), (Int32, Int64)] = @{(1i8, 2i16) -> (3i32, 4i64)}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap01") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap02") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapMap03") {
    val input = "def f: Map[Int, Map[Int, Char]] = @{1 -> @{}, 2 -> @{3 -> 'a', 4 -> 'b'}}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapList01") {
    val input = "def f: Map[Int, List[Int]] = @{1 -> 2 :: 3 :: Nil, 4 -> 5 :: 6 :: Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet01") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet02") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.MapListSet04") {
    val input = "def f: Map[Int, List[Set[Int]]] = @{1 -> #{1, 2, 3} :: #{4, 5, 6} :: Nil}"
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Expression.Var01") {
    val input = "def f(x: Int): Int = x"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda01") {
    val input = "def f: Int -> Int = x -> x"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda02") {
    val input = "def f: Int -> Int = (x) -> x"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda03") {
    val input = "def f: (Bool, Char) -> Int = (x, y) -> 42"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda04") {
    val input = "def f: (Bool, Char, Int) -> Int = (x, y, z) -> 42"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda05") {
    val input = "def f: (Int8, Int16, Int32, Int64) -> Int32 = (x, y, z, w) -> z"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda06") {
    val input = "def f: Int -> (Bool, Char) = x -> (true, 'a')"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda07") {
    val input = "def f: Int -> (Bool, Char, Int) = x -> (true, 'a', 42)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda08") {
    val input = "def f: (Bool, Char) -> (Char, Bool) = (x, y) -> (y, x)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda09") {
    val input = "def f: (Bool, Char, Int) -> (Int, Char, Bool) = (x, y, z) -> (z, y, x)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda10") {
    val input = "def f: ((Bool, Char), Int) -> (Bool, Char) = (x, y) -> x"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda11") {
    val input = "def f: (Bool, (Char, Int)) -> (Char, Int) = (x, y) -> y"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda12") {
    val input = "def f: (Int, Int) -> ((Int, Int), (Int, Int)) = x -> (x, x)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda13") {
    val input = "def f: Bool -> Char -> Int = x -> (y -> 42)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda14") {
    val input = "def f: (Bool, Bool) -> Char -> Int = (x1, x2) -> (y -> 42)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda15") {
    val input = "def f: Bool -> (Char, Char) -> Int = x -> ((y1, y2) -> 42)"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda16") {
    val input = "def f: Bool -> Char -> (Int, Int) = x -> (y -> (21, 42))"
    new Flix().addStr(input).compile().get
  }

  ignore("Expression.Lambda17") {
    val input = "def f: (Bool, Bool) -> (Char, Char) -> (Int, Int) = (x1, x2) -> ((y1, y2) -> (21, 42))"
    new Flix().addStr(input).compile().get
  }

  test("Expression.UserError01") {
    val input = "def f: Int = ???"
    new Flix().addStr(input).compile().get
  }

  test("Expression.Bot01") {
    val input = "⊥"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Bot])
  }

  test("Expression.Top01") {
    val input = "⊤"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Top])
  }

  test("Expression.Leq01") {
    val input = "x ⊑ y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Leq)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.Lub01") {
    val input = "x ⊔ y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Lub)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.Glb1") {
    val input = "x ⊓ y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Glb)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.Widen01") {
    val input = "x ▽ y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Widen)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.Narrow01") {
    val input = "x △ y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Narrow)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.BotLeqTop") {
    val input = "⊥ ⊑ ⊤"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.ExtendedBinary])
    assertResult(ExtBinaryOperator.Leq)(result.get.asInstanceOf[ParsedAst.Expression.ExtendedBinary].op)
  }

  test("Expression.Existential01") {
    val input = "∃(x: Bool). true"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Existential])
  }

  test("Expression.Existential02") {
    val input = "∃(x: Int, y: Int). x == y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Existential])
  }

  test("Expression.Existential03") {
    val input = "\\exists(x: Bool). true"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Existential])
  }

  test("Expression.Existential04") {
    val input = "\\exists(x: Int, y: Int). x == y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Existential])
  }

  test("Expression.Universal01") {
    val input = "∀(x: Bool). true"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Universal])
  }

  test("Expression.Universal02") {
    val input = "∀(x: Int, y: Int). x == y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Universal])
  }

  test("Expression.Universal03") {
    val input = "\\forall(x: Bool). true"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Universal])
  }

  test("Expression.Universal04") {
    val input = "\\forall(x: Int, y: Int). x == y"
    val result = new Parser(SourceInput.Str(input)).Expression.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Expression.Universal])
  }


  /////////////////////////////////////////////////////////////////////////////
  // Patterns                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Pattern.Wildcard") {
    val input = "_"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Wildcard])
  }

  test("Pattern.Var01") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Var]
    assertResult("x")(result.ident.name)
  }

  test("Pattern.Literal01") {
    val input = "true"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("true")(result.lit.asInstanceOf[ParsedAst.Literal.Bool].lit)
  }

  test("Pattern.Literal02") {
    val input = "42"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("42")(result.lit.asInstanceOf[ParsedAst.Literal.Int32].lit)
  }

  test("Pattern.Literal03") {
    val input = "\"foo\""
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Lit]
    assertResult("foo")(result.lit.asInstanceOf[ParsedAst.Literal.Str].lit)
  }

  test("Pattern.Tag01") {
    val input = "Const.Bot"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag02") {
    val input = "Const.Cst(5)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag03") {
    val input = "Foo.Bar (x, _, z)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tag04") {
    val input = "foo.bar/baz.Foo(x, y, z)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run()
    assert(result.isSuccess)
    assert(result.get.isInstanceOf[ParsedAst.Pattern.Tag])
  }

  test("Pattern.Tuple01") {
    val input = "(x, y, true)"
    val result = new Parser(SourceInput.Str(input)).Pattern.run().get.asInstanceOf[ParsedAst.Pattern.Tuple]
    assertResult(3)(result.pats.size)
  }

  test("Pattern.Opt01") {
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

  test("Pattern.Opt02") {
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

  test("Pattern.Opt03") {
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

  test("Pattern.List01") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List02") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List03") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List04") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case 1 :: 2 :: 3 :: Nil => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List05") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: Nil => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List06") {
    val input =
      """def f(xs: List[Int]): Int = match xs with {
        |  case x :: y :: Nil => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.List07") {
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

  test("Pattern.List08") {
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

  test("Pattern.List09") {
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

  test("Pattern.List10") {
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

  test("Pattern.List11") {
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

  test("Pattern.ListList01") {
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

  test("Pattern.ListList02") {
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

  test("Pattern.ListList03") {
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

  test("Pattern.Vec01") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec02") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec03") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec04") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[1, 2, 3] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec05") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x] => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec06") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y] => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec07") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z] => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Vec08") {
    val input =
      """def f(xs: Vec[Int]): Int = match xs with {
        |  case #[x, y, z, rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec01") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec02") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[1, 2, 3], #[4, 5, 6]] => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.VecVec03") {
    val input =
      """def f(xs: Vec[Vec[Int]]): Int = match xs with {
        |  case #[#[], #[1], #[1, 2, 3], #[x, y, z], rs...] => x + y + z + f(rs)
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set01") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set02") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set03") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set04") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{1, 2, 3, rs...} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set05") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set06") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set07") {
    val input =
      """def f(xs: Set[Int]): Int = match xs with {
        |  case #{x, y, z} => x + y + z
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Set08") {
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

  test("Pattern.Set09") {
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

  test("Pattern.SetSet01") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.SetSet02") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x}, #{y}, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.SetSet03") {
    val input =
      """def f(xs: Set[Set[Int]]): Int = match xs with {
        |  case #{#{x, y, as...}, #{z, w, bs...}, rs...} => x + y + z + w
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map01") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map02") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map03") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map04") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> 42, 'b' -> 21, c -> 11} => 0
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map05") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x} => x
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map06") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map07") {
    val input =
      """def f(xs: Map[Char, Int]): Int = match xs with {
        |  case @{'a' -> x, 'b' -> y, rs...} => x + y
        |}
      """.stripMargin
    intercept[scala.NotImplementedError] {
      new Flix().addStr(input).compile().get
    }
  }

  test("Pattern.Map08") {
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
  test("Declaration.Fact01") {
    val input =
      """rel R(a: Int)
        |R(42).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact02") {
    val input =
      """rel R(a: Char, b: Int)
        |R('a', 42).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Fact03") {
    val input =
      """rel R(a: Int8, b: Int16, c: Int32, d: Int64)
        |R(1i8, 2i16, 3i32, 4i64).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule01") {
    val input =
      """rel R(a: Int)
        |
        |R(x) :- R(x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule02") {
    val input =
      """rel R(a: Int)
        |
        |R(x) :- R(x), R(x), R(x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Declaration.Rule03") {
    val input =
      """rel R(a: Int, b: Int)
        |
        |R(x, y) :- R(x, y), R(y, x).
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Predicate.Equal01") {
    val input = "r := 42"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Equal])
  }

  ignore("Predicate.Equal02") {
    val input = "r := (true, 42, \"foo\")"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Equal])
  }

  test("Predicate.Equal03") {
    val input = "r := f(x, g(y, z))"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Equal])
  }

  test("Predicate.Loop01") {
    val input = "y <- f(x)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Loop])
  }

  test("Predicate.Loop02") {
    val input = "x <- f(1, 2, 3)"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate.Loop])
  }

  test("Predicate.NotEqual01") {
    val input = "x != y"
    val result = new Parser(SourceInput.Str(input)).Predicate.run().get
    assert(result.isInstanceOf[ParsedAst.Predicate])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Terms                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Term01") {
    val input = "_"
    val result = new Parser(SourceInput.Str(input)).Term.run().get
    assert(result.isInstanceOf[ParsedAst.Term.Wildcard])
  }

  test("Term02") {
    val input = "x"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Var]
    assertResult("x")(result.ident.name)
  }

  test("Term03") {
    val input = "42"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Lit]
    assertResult("42")(result.lit.asInstanceOf[ParsedAst.Literal.Int32].lit)
  }

  test("Term04") {
    val input = "foo(x)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    // assertResult(Seq("foo"))(result.name.parts)
  }

  test("Term05") {
    val input = "foo/bar(x, y, z)"
    val result = new Parser(SourceInput.Str(input)).Term.run().get.asInstanceOf[ParsedAst.Term.Apply]
    //assertResult(Seq("foo", "bar"))(result.name.parts)
    //assertResult(Seq("x", "y", "z"))(result.args.map(_.asInstanceOf[ParsedAst.Term.Var].ident.name))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Unit") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Type.Bool01") {
    val input = "def f: Bool = true"
    new Flix().addStr(input).compile().get
  }

  test("Type.Bool02") {
    val input = "def f: Bool = false"
    new Flix().addStr(input).compile().get
  }

  test("Type.Char") {
    val input = "def f: Char = 'a'"
    new Flix().addStr(input).compile().get
  }

  test("Type.Float32") {
    val input = "def f: Float32 = 0.0f32"
    new Flix().addStr(input).compile().get
  }

  test("Type.Float64") {
    val input = "def f: Float64 = 0.0f64"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int8") {
    val input = "def f: Int8 = 0i8"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int16") {
    val input = "def f: Int16 = 0i16"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int32") {
    val input = "def f: Int32 = 0i32"
    new Flix().addStr(input).compile().get
  }

  test("Type.Int64") {
    val input = "def f: Int64 = 0i64"
    new Flix().addStr(input).compile().get
  }

  test("Type.Str") {
    val input = "def f: Str = \"foobar\'"
    new Flix().addStr(input).compile().get
  }

  test("Type.Prop") {
    val input = "def f: Prop = true"
    new Flix().addStr(input).compile().get
  }

  test("Type.Enum") {
    val input =
      """enum Color {
        |  case Red
        |}
        |
        |def f: Color = Color.Red
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Type.Tuple01") {
    val input = "def f: (Int, Int) = (1, 2)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Tuple02") {
    val input = "def f: (Unit, Bool, Char, Int) = ((), true, 'a', 42)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Lambda01") {
    val input = "def f: Int = (x -> x + 1)(42)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Lambda02") {
    val input = "def f: Int = ((x, y) -> x + y)(21, 42)"
    new Flix().addStr(input).compile().get
  }

  test("Type.Parametric01") {
    val input = "def f(x: A): A = x"
    new Flix().addStr(input).compile().get
  }

  test("Type.Parametric02") {
    val input = "def f(x: A, y: B): A = x"
    new Flix().addStr(input).compile().get
  }

  test("Type.Opt") {
    val input = "def f: Opt[Int] = None"
    new Flix().addStr(input).compile().get
  }

  test("Type.List") {
    val input = "def f: List[Int] = Nil"
    new Flix().addStr(input).compile().get
  }

  test("Type.Vec") {
    val input = "def f: Vec[Int] = #[]"
    new Flix().addStr(input).compile().get
  }

  test("Type.Set") {
    val input = "def f: Set[Int] = #{}"
    new Flix().addStr(input).compile().get
  }

  test("Type.Map") {
    val input = "def f: Map[Int, Int] = @{}"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Identifiers & Names                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("Ident01") {
    val input = "def x: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident02") {
    val input = "def xx: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident03") {
    val input = "def xxx: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident04") {
    val input = "def xY: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident05") {
    val input = "def xxxYyy: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident06") {
    val input = "def xxxYyyZzz: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident07") {
    val input = "def x0: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident08") {
    val input = "def x0123: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident09") {
    val input = "def x_y_z: Int = 42"
    new Flix().addStr(input).compile().get
  }

  test("Ident10") {
    val input = "def x_Y32Y_15zz: Int = 42"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Literal.Unit") {
    val input = "def f: Unit = ()"
    new Flix().addStr(input).compile().get
  }

  test("Literal.True") {
    val input = "def f: Bool = true"
    new Flix().addStr(input).compile().get
  }

  test("Literal.False") {
    val input = "def f: Bool = false"
    new Flix().addStr(input).compile().get
  }

  test("Literal.Char") {
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

  test("Operator.ExtendedBinary.Leq ⊑") {
    val input = "⊑"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operators.ExtBinaryOpt).get
    assertResult(ExtBinaryOperator.Leq)(result)
  }

  test("Operator.ExtendedBinary.Lub ⊔") {
    val input = "⊔"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operators.ExtBinaryOpt).get
    assertResult(ExtBinaryOperator.Lub)(result)
  }

  test("Operator.ExtendedBinary.Glb ⊓") {
    val input = "⊓"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operators.ExtBinaryOpt).get
    assertResult(ExtBinaryOperator.Glb)(result)
  }

  test("Operator.ExtendedBinary.Widen ▽") {
    val input = "▽"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operators.ExtBinaryOpt).get
    assertResult(ExtBinaryOperator.Widen)(result)
  }

  test("Operator.ExtendedBinary.Narrow △") {
    val input = "△"
    val parser = mkParser(input)
    val result = parser.__run(parser.Operators.ExtBinaryOpt).get
    assertResult(ExtBinaryOperator.Narrow)(result)
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
  test("Annotation01") {
    val input =
      """@strict
        |fn f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation02") {
    val input =
      """@monotone
        |fn f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation03") {
    val input =
      """@strict @monotone
        |fn f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Annotation04") {
    val input =
      """@strict @monotone @commutative @associative @unsafe @unchecked
        |fn f(x: Int): Int = x
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Whitespace                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("WhiteSpace01") {
    val input = " "
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace02") {
    val input = "    "
    new Flix().addStr(input).compile().get
  }

  test("WhiteSpace03") {
    val input = "\t"
    new Flix().addStr(input).compile().get
  }

  ignore("WhiteSpace04") {
    val input = "\n\r"
    new Flix().addStr(input).compile().get
  }

  /////////////////////////////////////////////////////////////////////////////
  // Comments                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("SingleLineComment01") {
    val input = "// a comment"
    new Flix().addStr(input).compile().get
  }

  test("SingleLineComment02") {
    val input =
      """// a comment
        |// another comment
        |// and yet another
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("MultiLineComment01") {
    val input = "/* a comment */"
    new Flix().addStr(input).compile().get
  }

  test("MultiLineComment02") {
    val input =
      """/*
        |a comment
        |*/
      """.stripMargin
    new Flix().addStr(input).compile().get
  }

  test("Comment01") {
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

  test("Comment02") {
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

  /**
    * Returns a parser for the given string `s`.
    */
  private def mkParser(s: String): Parser = new Parser(SourceInput.Str(s))
}
