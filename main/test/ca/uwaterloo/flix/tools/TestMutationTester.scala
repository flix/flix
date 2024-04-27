package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.MutationTester.MutantStatus.{CompilationFailed, Killed, Survived, TimedOut}
import ca.uwaterloo.flix.tools.MutationTester.{MutantRunner, MutantStatus, Mutator}
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.concurrent.duration.DurationInt

class TestMutationTester extends AnyFunSuite {

  private val mutantTestTimeout = 1.seconds
  private val sourceTestsFailedMsg = "Source tests failed"

  testSource(
    "Test.SourceTests.Successed",
    """
      |@Test
      |def test(): Bool = Assert.eq(true, true)
      |""".stripMargin,
    Result.Ok(()),
  )

  testSource(
    "Test.SourceTests.Failed",
    """
      |@Test
      |def test(): Bool = Assert.eq(true, false)
      |""".stripMargin,
    Result.Err(sourceTestsFailedMsg),
  )

  mkMutationTest(
    "Test.EmptySource",
    """
      |@Test
      |def test(): Bool = Assert.eq(true, true)
      |""".stripMargin,
    Map.empty
  )

  mkMutationTest(
    "Test.Constant.Boolean.False",
    """
      |def foo(): Bool = false
      |
      |@Test
      |def test(): Bool = Assert.eq(false, foo())
      |""".stripMargin,
    Map(
      Killed -> 1,
    )
  )

  mkMutationTest(
    "Test.Constant.Boolean.True",
    """
      |def foo(): Bool = true
      |
      |@Test
      |def test(): Bool = Assert.eq(true, foo())
      |""".stripMargin,
    Map(
      Killed -> 1,
    )
  )

  mkMutationTest(
    "Test.Constant.Number.Float32",
    """
      |def foo(): Float32 = 10.0f32
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0f32, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.Float64",
    """
      |def foo(): Float64 = 10.0
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.BigDecimal",
    """
      |def foo(): BigDecimal = 10.0ff
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0ff, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    testName = "Test.Constant.Number.Int8",
    """
      |def foo(): Int8 = 10i8
      |
      |@Test
      |def test(): Bool = Assert.eq(10i8, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.Int16",
    """
      |def foo(): Int16 = 10i16
      |
      |@Test
      |def test(): Bool = Assert.eq(10i16, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.Int32",
    """
      |def foo(): Int32 = 10
      |
      |@Test
      |def test(): Bool = Assert.eq(10, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.Int64",
    """
      |def foo(): Int64 = 10i64
      |
      |@Test
      |def test(): Bool = Assert.eq(10i64, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.BigInt",
    """
      |def foo(): BigInt = 10ii
      |
      |@Test
      |def test(): Bool = Assert.eq(10ii, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.String.NonEmpty",
    """
      |def foo(): String = "Hello, world!"
      |
      |@Test
      |def test(): Bool = Assert.eq("Hello, world!", foo())
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Constant.String.Empty",
    """
      |def foo(): String = ""
      |
      |@Test
      |def test(): Bool = Assert.eq("", foo())
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Unary.Boolean.Not",
    """
      |def foo(x: Bool): Bool = not x
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(false))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(true))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Unary.Arithmetic.Neg.01",
    """
      |def foo(x: Int32): Int32 = -x
      |
      |@Test
      |def test1(): Bool = Assert.eq(-10, foo(10))
      |
      |@Test
      |def test2(): Bool = Assert.eq(10, foo(-10))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Unary.Arithmetic.Neg.02",
    """
      |def foo(x: Int32): Int32 = Neg.neg(x)
      |
      |@Test
      |def test1(): Bool = Assert.eq(-10, foo(10))
      |
      |@Test
      |def test2(): Bool = Assert.eq(10, foo(-10))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Unary.Bitwise.Not",
    """
      |def foo(x: Int32): Int32 = Int32.bitwiseNot(x)
      |
      |@Test
      |def test(): Bool = Assert.eq(-124, foo(123))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Boolean.Or",
    """
      |def foo(x: Bool, y: Bool): Bool = x or y
      |
      |@Test
      |def test(): Bool = Assert.eq(true , foo(false, true))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Boolean.And",
    """
      |def foo(x: Bool, y: Bool): Bool = x and y
      |
      |@Test
      |def test(): Bool = Assert.eq(false , foo(false, true))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Add.01",
    """
      |def foo(a: Float64, b: Float64): Float64 = a + b
      |
      |@Test
      |def test(): Bool = Assert.eq(5.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Add.02",
    """
      |def foo(a: Float64, b: Float64): Float64 = Add.add(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(5.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )


  mkMutationTest(
    "Test.Binary.Arithmetic.AddStr.01",
    """
      |def foo(a: String, b: String): String = a + b
      |
      |@Test
      |def test(): Bool = Assert.eq("Hello, world!", foo("Hello, ", "world!"))
      |""".stripMargin,
    Map.empty,
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.AddStr.02",
    """
      |def foo(a: String, b: String): String = Add.add(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq("Hello, world!", foo("Hello, ", "world!"))
      |""".stripMargin,
    Map.empty,
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Sub.01",
    """
      |def foo(a: Float64, b: Float64): Float64 = a - b
      |
      |@Test
      |def test(): Bool = Assert.eq(1.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Sub.02",
    """
      |def foo(a: Float64, b: Float64): Float64 = Sub.sub(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(1.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Mul.01",
    """
      |def foo(a: Float64, b: Float64): Float64 = a * b
      |
      |@Test
      |def test(): Bool = Assert.eq(6.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Mul.02",
    """
      |def foo(a: Float64, b: Float64): Float64 = Mul.mul(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(6.0, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Div.01",
    """
      |def foo(a: Float64, b: Float64): Float64 = a / b
      |
      |@Test
      |def test(): Bool = Assert.eq(1.5, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Div.02",
    """
      |def foo(a: Float64, b: Float64): Float64 = Div.div(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(1.5, foo(3.0, 2.0))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Mod",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.modulo(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(1, foo(-9, 5))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Arithmetic.Rem",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.remainder(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(-4, foo(-9, 5))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Bitwise.And",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.bitwiseAnd(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(4, foo(6, 5))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Bitwise.Or",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.bitwiseOr(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(7, foo(6, 5))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Bitwise.Xor",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.bitwiseXor(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(3, foo(6, 5))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Bitwise.Rsh",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.rightShift(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(2, foo(8, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Bitwise.Lsh",
    """
      |def foo(a: Int32, b: Int32): Int32 = Int32.leftShift(a, b)
      |
      |@Test
      |def test(): Bool = Assert.eq(32, foo(8, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Eq.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a == b
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Eq.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Eq.eq(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Neq.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a != b
      |
      |@Test
      |def test1(): Bool = Assert.eq(false, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(true, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Neq.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Eq.neq(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(false, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(true, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Gt.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a > b
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(3, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(2, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Gt.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Order.greater(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(3, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(2, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Lt.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a < b
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 3))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(2, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Lt.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Order.less(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 3))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(2, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Ge.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a >= b
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(1, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Ge.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Order.greaterEqual(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(1, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Le.01",
    """
      |def foo(a: t, b: t): Bool with Order[t] = a <= b
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Binary.Conditional.Le.02",
    """
      |def foo(a: t, b: t): Bool with Order[t] = Order.lessEqual(a, b)
      |
      |@Test
      |def test1(): Bool = Assert.eq(true, foo(2, 2))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, foo(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Boolean.Combined",
    """
      |def foo(): Bool = (true or (3 >= 2)) and (not false or (2 != 3))
      |""".stripMargin,
    Map(
      Survived -> 7,
    ),
  )

  mkMutationTest(
    "Test.Arithmetic.Combined",
    """
      |def foo(): Int32 = (2 + Int32.bitwiseAnd(12, 14)) * (Int32.bitwiseNot(-15) - 4)
      |""".stripMargin,
    Map(
      Survived -> 5,
    ),
  )

  mkMutationTest(
    "Test.IfThenElse.01",
    """
      |def f1(n: Int32): Bool = if (n >= 20) true else false
      |""".stripMargin,
    Map(
      Survived -> 4,
    ),
  )

  mkMutationTest(
    "Test.IfThenElse.02",
    """
      |def f2(x: Int32, y: Int32): Int32 = if (x == 5) {
      |    if (y == 7) {
      |        123
      |    } else {
      |        231
      |    }
      |} else {
      |    312
      |}
      |
      |@Test
      |def test1(): Bool = Assert.eq(231 , f2(5, 5))
      |
      |@Test
      |def test2(): Bool = Assert.eq(123 , f2(5, 7))
      |
      |@Test
      |def test3(): Bool = Assert.eq(312 , f2(7, 5))
      |
      |@Test
      |def test4(): Bool = Assert.eq(312 , f2(7, 7))
      |""".stripMargin,
    Map(
      Killed -> 8,
    ),
  )

  mkMutationTest(
    "Test.PatternMatching.01",
    """
      |enum Animal {
      |    case Cat,
      |    case Dog,
      |    case Giraffe
      |}
      |
      |def isTall(a: Animal): Bool = match a {
      |    case Animal.Cat        => false
      |    case Animal.Dog        => false
      |    case Animal.Giraffe    => true
      |}
      |
      |@Test
      |def test1(): Bool = Assert.eq(false, isTall(Animal.Cat))
      |
      |@Test
      |def test2(): Bool = Assert.eq(false, isTall(Animal.Dog))
      |
      |@Test
      |def test3(): Bool = Assert.eq(true, isTall(Animal.Giraffe))
      |""".stripMargin,
    Map(
      Killed -> 3,
    ),
  )

  mkMutationTest(
    "Test.PatternMatching.02",
    """
      |enum Shape {
      |    case Circle({ radius = Int32 })
      |    case Square({ width = Int32 })
      |    case Rectangle({ height = Int32, width = Int32 })
      |}
      |
      |def area(s: Shape): Int32 = match s {
      |    case Shape.Circle({ radius })           => 3 * (radius * radius)
      |    case Shape.Square({ width })            => width * width
      |    case Shape.Rectangle({ height, width }) => height * width
      |}
      |
      |@Test
      |def test1(): Bool = Assert.eq(12, area(Shape.Circle({radius = 2})))
      |
      |@Test
      |def test2(): Bool = Assert.eq(16, area(Shape.Square({width = 4})))
      |
      |@Test
      |def test3(): Bool = Assert.eq(15, area(Shape.Rectangle({height = 3, width = 5})))
      |""".stripMargin,
    Map(
      Killed -> 4,
    ),
  )

  mkMutationTest(
    "Test.Tuple.01",
    """
      |def foo(): (Int32, String) = (1, "Dibgashi")
      |
      |@Test
      |def test(): Bool = Assert.eq((1, "Dibgashi"), foo())
      |""".stripMargin,
    Map(
      Killed -> 3,
    ),
  )

  mkMutationTest(
    "Test.Tuple.02",
    """
      |def foo(): (Bool, (Int32, Bool)) = (true, (3 + 2, 4.0 > 3.9))
      |
      |@Test
      |def test(): Bool = Assert.eq((true, (5, true)), foo())
      |""".stripMargin,
    Map(
      Killed -> 3,
      Survived -> 1,
    ),
  )

  mkMutationTest(
    "Test.List.01",
    """
      |def foo(): List[String] = "Hello" :: "World" :: Nil
      |
      |@Test
      |def test(): Bool = Assert.eq("Hello" :: "World" :: Nil, foo())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.List.02",
    """
      |def length(): Int32 = List.length(2 :: 3 :: Nil)
      |
      |def reverse(): List[Int32] = List.reverse(2 :: 3 :: Nil)
      |
      |@Test
      |def testLength(): Bool = Assert.eq(2, length())
      |
      |@Test
      |def testReverse(): Bool = Assert.eq(3 :: 2 :: Nil, reverse())
      |""".stripMargin,
    Map(
      Killed -> 4,
      Survived -> 4,
    ),
  )

  mkMutationTest(
    "Test.Vector.01",
    """
      |def foo(): Vector[Int32] = Vector#{1, 2}
      |
      |@Test
      |def test(): Bool = Assert.eq(Vector#{1, 2}, foo())
      |""".stripMargin,
    Map(
      Killed -> 4,
    ),
  )

  mkMutationTest(
    "Test.Set.01",
    """
      |def foo(): Set[Int32] = Set#{2, 13, 249}
      |
      |@Test
      |def test(): Bool = Assert.eq(Set#{2, 13, 249}, foo())
      |""".stripMargin,
    Map(
      Killed -> 6,
    ),
  )

  mkMutationTest(
    "Test.Map.01",
    """
      |def foo(): Map[Int32, Bool] = Map#{2 => true, 13 => false, 249 => true}
      |
      |@Test
      |def test(): Bool = Assert.eq(Map#{2 => true, 13 => false, 249 => true}, foo())
      |""".stripMargin,
    Map(
      Killed -> 9,
    ),
  )


  mkMutationTest(
    "Test.FixpointConstraintSet.01",
    """
      |def foo(): #{ City() } = #{
      |    City().
      |}
      |""".stripMargin,
    Map(
      Survived -> 1,
    ),
  )

  mkMutationTest(
    "Test.FixpointConstraintSet.02",
    """
      |def foo(): #{ City(String) } = #{
      |    City("London").
      |    City("Paris").
      |    City("Moscow").
      |}
      |""".stripMargin,
    Map(
      Survived -> 6,
    ),
  )

  mkMutationTest(
    "Test.FixpointConstraintSet.03",
    """
      |def foo(): #{ ParentOf(String, String),
      |          AncestorOf(String, String)} = #{
      |    AncestorOf(x, y) :- ParentOf(x, y).
      |    AncestorOf(x, z) :- AncestorOf(x, y), AncestorOf(y, z).
      |}
      |""".stripMargin,
    Map(
      CompilationFailed -> 3,
      Survived -> 5,
    ),
  )

  mkMutationTest(
    "Test.FixpointConstraintSet.04",
    """
      |def f2(): Vector[(String, Int32)] =
      |    let rules = #{
      |        A("a"; 1).
      |        B(1).
      |
      |        C(s; a + b) :- A(s; a), B(b).
      |    };
      |
      |    query rules select (s, n) from C(s; n)
      |""".stripMargin,
    Map(
      CompilationFailed -> 2,
      Survived -> 11,
    ),
  )

  mkMutationTest(
    "Test.FixpointConstraintSet.05",
    """
      |def foo(roads: List[(city, city)]): Vector[(city, city)] with Order[city] =
      |    let r = inject roads into Road;
      |    let lp = #{
      |        City(x) :- Road(x, _).
      |        City(y) :- Road(_, y).
      |        Path(x, y) :- Road(x, y).
      |        Path(x, z) :- Path(x, y), Road(y, z).
      |        Unconnected(x, y) :- City(x), City(y), not Path(x, y).
      |    };
      |
      |    query r, lp select (x, y) from Unconnected(x, y)
      |
      |@Test
      |def test(): Bool = Assert.eq(Vector#{(1, 2), (2, 1)} , foo((1, 1) :: (2, 2) :: Nil))
      |""".stripMargin,
    Map(
      CompilationFailed -> 9,
      Killed -> 9,
      Survived -> 3,
    ),
  )

  mkMutationTest(
    "Test.FixpointConstraintSet.06",
    """
      |def isPrime(_: Int32): Bool = true
      |
      |def primesInRange(b: Int32, e: Int32): Vector[Int32] =
      |    Vector.range(b, e) |> Vector.filter(isPrime)
      |
      |def specific(): Vector[Int32]=
      |    let r = #{
      |        P(5).
      |
      |        R(p) :- P(b), let p = primesInRange(b, 9), if b >= 5.
      |    };
      |
      |    query r select (p) from R(p)
      |
      |@Test
      |pub def testSpecific1(): Bool = Assert.eq(Vector#{5, 6, 7, 8}, specific())
      |""".stripMargin,
    Map(
      CompilationFailed -> 2,
      Killed -> 10,
      Survived -> 1,
    ),
  )

  mkMutationTest(
    "Test.Type.Unit.Returned",
    """
      |def foo(): Unit \ IO =
      |    let n = 1;
      |    println(n)
      |""".stripMargin,
    Map.empty
  )

  mkMutationTest(
    "Test.Type.Unit.Called",
    """
      |def foo(x: Bool): Bool \ IO =
      |    println("This string will not be mutated");
      |    x
      |""".stripMargin,
    Map.empty
  )

  mkMutationTest(
    "Test.Transitive.Call",
    """
      |def foo(x: Bool): Bool = not x
      |
      |def bar(): Bool = foo(true)
      |
      |@Test
      |def test(): Bool = Assert.eq(false, bar())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Instance.Def",
    """
      |trait MyAdd[t] {
      |    pub def myAdd(x: t, y: t): t
      |}
      |
      |instance MyAdd[Int32] {
      |    pub def myAdd(x: Int32, y: Int32): Int32 = x + y
      |}
      |
      |@Test
      |def test(): Bool = Assert.eq(5, MyAdd.myAdd(3, 2))
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Ann.Benchmark",
    """
      |@benchmark
      |pub def benchmark01(): Bool = List.length(List.range(0, 1000)) == 1000
      |""".stripMargin,
    Map.empty,
  )

  mkMutationTest(
    "Test.Ann.Skip",
    """
      |def foo(): Bool = true
      |
      |@Test
      |@Skip
      |def test(): Bool = Assert.eq(true, foo())
      |""".stripMargin,
    Map(
      Survived -> 1,
    ),
  )

  mkMutationTest(
    "Test.MutantStatus.TimedOut",
    s"""
       |def foo(): Bool = true
       |
       |@Test
       |def test(): Bool \\ IO =
       |    Thread.sleep(Time.Duration.fromSeconds(${mutantTestTimeout.toSeconds + 1}));
       |    Assert.eq(true, foo())
       |""".stripMargin,
    Map(
      TimedOut -> 1,
    ),
  )

  mkMutationTest(
    "Test.Example.Imperative.With.Effect",
    """
      |def f1(k: Int32): Int32 \ IO =
      |    let a = 10;
      |    let b = 20;
      |    println("(a + b) * k");
      |    (a + b) * k
      |
      |def f2(): Int32 \ IO = f1(3)
      |
      |@Test
      |def test1(): Bool\ IO = Assert.eq(30, f1(1))
      |
      |@Test
      |def test2(): Bool\ IO = Assert.eq(60, f1(2))
      |
      |@Test
      |def test3(): Bool\ IO = Assert.eq(90, f2())
      |""".stripMargin,
    Map(
      Killed -> 8,
    ),
  )

  mkMutationTest(
    "Test.Example.Without.Mutants",
    """
      |def reachable(origin: n, edges: es): Set[n] \ Iterable.Aef[es] with Iterable[es], Order[n] where Iterable.Elm[es] ~ (n, n) =
      |    region rc {
      |        // collect edge lists
      |        let edgeMap = MutMap.empty(rc);
      |        foreach((start, end) <- edges) {
      |            let startEdges = MutMap.getOrElsePut!(start, MutList.empty(rc), edgeMap);
      |            MutList.push!(end, startEdges)
      |        };
      |        // define the reachable set
      |        let reachable = MutSet.empty(rc);
      |        // explore graph depth first by task list
      |        let taskList = MutDeque.empty(rc);
      |        taskList |> MutDeque.pushFront(origin);
      |        def whileLoop() = {
      |            MutDeque.popFront(taskList) |> Option.forEach(node -> {
      |                // this node has now been reached
      |                reachable |> MutSet.add!(node);
      |                // add all non-reached end points to tasklist
      |                let endPoints = MutMap.getWithDefault(node, MutList.empty(rc), edgeMap);
      |                MutList.forEach(nextNode -> {
      |                    let alreadyReached = MutSet.memberOf(nextNode, reachable);
      |                    if (not alreadyReached)
      |                        taskList |> MutDeque.pushFront(nextNode)
      |                    else ()
      |                }, endPoints);
      |                whileLoop()
      |            })
      |        };
      |        whileLoop();
      |        MutSet.toSet(reachable)
      |    }
      |""".stripMargin,
    Map.empty,
  )

  mkMutationTest(
    "Test.Example.FixpointConstraintSet.Before",
    """
      |def isConnected(s: Set[(Int32, Int32)], src: Int32, dst: Int32): Bool =
      |    let rules = #{
      |        Path(x, y) :- Edge(x, y).
      |        Path(x, z) :- Path(x, y), Edge(y, z).
      |    };
      |    let edges = inject s into Edge;
      |    let paths = query edges, rules select true from Path(src, dst);
      |    not (paths |> Vector.isEmpty)
      |
      |@Test
      |pub def testIsConnected(): Bool = Assert.eq(true, isConnected(Set#{(1, 2), (2, 3)}, 1, 2))
      |""".stripMargin,
    Map(
      CompilationFailed -> 3,
      Killed -> 5,
      Survived -> 1,
    ),
  )

  mkMutationTest(
    "Test.Example.FixpointConstraintSet.After",
    """
      |def isConnected(s: Set[(Int32, Int32)], src: Int32, dst: Int32): Bool =
      |    let rules = #{
      |        Path(x, y) :- Edge(x, y).
      |        Path(x, z) :- Path(x, y), Edge(y, z).
      |    };
      |    let edges = inject s into Edge;
      |    let paths = query edges, rules select true from Path(src, dst);
      |    not (paths |> Vector.isEmpty)
      |
      |@Test
      |pub def testIsConnected1(): Bool = Assert.eq(true, isConnected(Set#{(1, 2), (2, 3), (3, 4)}, 1, 4))
      |
      |@Test
      |pub def testIsConnected2(): Bool = Assert.eq(false, isConnected(Set#{(1, 2), (2, 3), (3, 4)}, 4, 1))
      |
      |@Test
      |pub def testIsConnected3(): Bool = Assert.eq(false, isConnected(Set#{(1, 2), (2, 3), (3, 4)}, 1, 1))
      |""".stripMargin,
    Map(
      CompilationFailed -> 3,
      Killed -> 6,
    ),
  )

  mkMutationTest(
    "Test.Example.Region.Without.Mutants",
    """
      |def sort(l: List[a]): List[a] with Order[a] =
      |    region rc {
      |        let arr = List.toArray(rc, l);
      |        Array.sort!(arr);
      |        Array.toList(arr)
      |    }
      |
      |@Test
      |def test1(): Bool = Assert.eq(List#{1, 2, 3}, sort(List#{3, 1, 2}))
      |
      |@Test
      |def test2(): Bool = Assert.eq(List#{2, 4, 6, 8}, sort(List#{6, 4, 8, 2}))
      |""".stripMargin,
    Map.empty,
  )

  private def testSource(testName: String, input: String, expected: Result[Unit, String]): Unit = {
    testHelper(testName, input, { cr => { _ =>
      assert(expected == MutantRunner.testSource(cr).map { _ => () })
    }
    })
  }

  private def mkMutationTest(testName: String, input: String, expectedStatuses: Map[MutantStatus, Int]): Unit = {
    testHelper(testName, input, { _ => { flix =>
      val actualStatuses = Mutator.mutateRoot(flix.getTyperAst)(flix)
        .map(m => MutantRunner.processMutant(m.value, mutantTestTimeout)(flix))
        .groupBy(identity)
        .view.mapValues(_.length)
        .toMap

      assert(actualStatuses == expectedStatuses)
    }
    })
  }

  private def testHelper(testName: String, input: String, assertF: CompilationResult => Flix => Unit): Unit = {
    test(testName) {
      implicit val flix: Flix = new Flix()

      flix.setOptions(Options.TestWithLibAll)

      val path = createTempFileWithText(input)

      flix.addFlix(path)

      try {
        flix.compile().toHardResult match {
          case Result.Ok(cr) => assertF(cr)(flix)
          case Result.Err(errors) =>
            val es = errors.map(_.message(flix.getFormatter)).mkString("\n")
            fail(s"Unable to compile. Failed with: ${errors.length} errors.\n\n$es")
        }
      } finally {
        Files.delete(path)
      }
    }
  }

  private def createTempFileWithText(text: String): Path = {
    val tempFilePath = Files.createTempFile("tempfile", ".flix")
    val writer = new PrintWriter(tempFilePath.toFile)
    writer.write(text)
    writer.close()
    tempFilePath
  }
}
