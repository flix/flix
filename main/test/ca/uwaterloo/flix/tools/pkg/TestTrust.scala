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

  private val Main: String =
    """
      |pub def main(): Unit \ IO =
      |    TestPkgTrust.entry()
      |""".stripMargin

  test("trust:plain-dep:plain") {
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
      case Result.Err(error) => fail(error.message(formatter))
    }
  }

  test("trust:plain-dep:java") {
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
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin

    FileOps.writeString(path.resolve("flix.toml"), toml)
    FileOps.writeString(path.resolve("src/").resolve("Main.flix"), Main)

    Bootstrap.bootstrap(path, None).flatMap {
      bootstrap =>
        val flix = new Flix()
        bootstrap.check(flix)
    } match {
      case Result.Ok(_) => fail("expected failure with trust 'plain' and dependency using Java")
      case Result.Err(_) =>
        // TODO: Check that error is forbidden / safety error
        succeed
    }
  }

  test("trust:plain-dep:unchecked-cast") {
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
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin

    FileOps.writeString(path.resolve("flix.toml"), toml)
    FileOps.writeString(path.resolve("src/").resolve("Main.flix"), Main)

    Bootstrap.bootstrap(path, None).flatMap {
      bootstrap =>
        val flix = new Flix()
        bootstrap.check(flix)
    } match {
      case Result.Ok(_) => fail("expected failure with trust 'plain' and dependency using unchecked cast")
      case Result.Err(_) =>
        // TODO: Check that error is forbidden / safety error
        succeed
    }
  }

  test("trust:plain-dep:java-unchecked-cast") {
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
        |"github:flix/test-pkg-trust-java-unchecked-cast" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin

    FileOps.writeString(path.resolve("flix.toml"), toml)
    FileOps.writeString(path.resolve("src/").resolve("Main.flix"), Main)

    Bootstrap.bootstrap(path, None).flatMap {
      bootstrap =>
        val flix = new Flix()
        bootstrap.check(flix)
    } match {
      case Result.Ok(_) => fail("expected failure with trust 'plain' and dependency using Java and unchecked cast")
      case Result.Err(_) =>
        // TODO: Check that error is forbidden / safety error
        succeed
    }
  }
}
