package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.tools.MutationTester.MutantStatus.{Killed, Survived}
import ca.uwaterloo.flix.tools.MutationTester.{MutantRunner, MutantStatus, Mutator}
import ca.uwaterloo.flix.util.{Options, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.concurrent.duration.DurationInt

class TestMutationTester extends AnyFunSuite {

  private val mutantTestTimeout = 2.seconds

  mkMutationTest(
    "Test.Constant.Boolean.01",
    """
      |def f1(): Bool = false
      |
      |@Test
      |def test(): Bool = Assert.eq(false, f1())
      |""".stripMargin,
    Map(
      Killed -> 1,
    )
  )

  mkMutationTest(
    "Test.Constant.Boolean.02",
    """
      |def f1(): Bool = true
      |
      |@Test
      |def test(): Bool = Assert.eq(true, f1())
      |""".stripMargin,
    Map(
      Killed -> 1,
    )
  )

  mkMutationTest(
    "Test.Constant.Number.01",
    """
      |def f1(): Float32 = 10.0f32
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0f32, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.02",
    """
      |def f1(): Float64 = 10.0
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.03",
    """
      |def f1(): BigDecimal = 10.0ff
      |
      |@Test
      |def test(): Bool = Assert.eq(10.0ff, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    testName = "Test.Constant.Number.04",
    """
      |def f1(): Int8 = 10i8
      |
      |@Test
      |def test(): Bool = Assert.eq(10i8, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.05",
    """
      |def f1(): Int16 = 10i16
      |
      |@Test
      |def test(): Bool = Assert.eq(10i16, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.06",
    """
      |def f1(): Int32 = 10
      |
      |@Test
      |def test(): Bool = Assert.eq(10, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.07",
    """
      |def f1(): Int64 = 10i64
      |
      |@Test
      |def test(): Bool = Assert.eq(10i64, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.Number.08",
    """
      |def f1(): BigInt = 10ii
      |
      |@Test
      |def test(): Bool = Assert.eq(10ii, f1())
      |""".stripMargin,
    Map(
      Killed -> 2,
    ),
  )

  mkMutationTest(
    "Test.Constant.String.01",
    """
      |def f1(): String = "Hello, world!"
      |
      |@Test
      |def test(): Bool = Assert.eq("Hello, world!", f1())
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  mkMutationTest(
    "Test.Constant.String.02",
    """
      |def f1(): String = ""
      |
      |@Test
      |def test(): Bool = Assert.eq("", f1())
      |""".stripMargin,
    Map(
      Killed -> 1,
    ),
  )

  private def mkMutationTest(testName: String, input: String, expectedStatuses: Map[MutantStatus, Int]): Unit = {
    test(testName) {
      implicit val flix: Flix = new Flix()

      flix.setOptions(Options.TestWithLibAll)

      val path = createTempFileWithText(input)

      flix.addFlix(path)

      try {
        flix.compile().toHardResult match {
          case Result.Ok(_) =>
            val actualStatuses = Mutator.mutateRoot(flix.getTyperAst)
              .map(m => MutantRunner.processMutant(m.value, mutantTestTimeout))
              .groupBy(identity)
              .view.mapValues(_.length)
              .toMap

            assert(actualStatuses == expectedStatuses)
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
