package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.MutationTester.MutantStatus.{Killed, Survived, TimedOut}
import ca.uwaterloo.flix.tools.MutationTester.{MutantRunner, MutantStatus, Mutator}
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.concurrent.duration.DurationInt

class TestMutationTester extends AnyFunSuite {

  private val mutantTestTimeout = 2.seconds
  private val sourceTestsFailedMsg = "Source tests failed"

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

  //   ---------------------------------------------------------------------
  //   todo: reorder

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
    "Test.Transitive",
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
    "Test.MutantStatus.TimeOut",
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

  private def testSource(testName: String, input: String, expected: Result[Unit, String]): Unit = {
    testHelper(testName, input, { cr => { _ =>
      assert(expected == MutantRunner.testSource(cr).map { _ => () })
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
