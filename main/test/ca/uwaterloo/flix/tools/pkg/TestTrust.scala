package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.util.{FileOps, Formatter, Result}
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintStream
import java.nio.file.Files

class TestTrust extends AnyFunSuite {
  test("toString-ofString-plain") {
    val perm = Trust.Plain
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-trust-javaclass") {
    val perm = Trust.TrustJavaClass
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  test("toString-ofString-unrestricted") {
    val perm = Trust.Unrestricted
    val res = Trust.fromString(perm.toString) match {
      case Some(r) => r
      case None => fail()
    }
    assertResult(perm)(res)
  }

  val Main =
    """
      |pub def main(): Unit \ IO =
      |    TestPkgTrust.entrypoint()
      |""".stripMargin

  test("trust-plain-plain") {
    implicit val out: PrintStream = System.out
    implicit val formatter: Formatter = Formatter.NoFormatter
    val path = Files.createTempDirectory("")
    val toml =
      """
        |[package]
        |name = "test"
        |description = "test"
        |version = "0.1.0"
        |flix = "0.65.0"
        |authors = ["flix"]
        |
        |[dependencies]
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin

    FileOps.writeString(path.resolve("flix.toml"), toml)
    FileOps.writeString(path.resolve("src/").resolve("Main.flix"), Main)

    Bootstrap.bootstrap(path, None).flatMap {
      bootstrap =>
        val flix = new Flix()
        bootstrap.check(flix)
    } match {
      case Result.Ok(_) => succeed
      case Result.Err(errors) => fail(errors.message(formatter))
    }
  }
}
