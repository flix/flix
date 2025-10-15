package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Formatter
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintStream
import java.nio.file.{Files, Path}

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

  test("trust:plain-dep:plain") {
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

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      fail(message)
    } else {
      succeed
    }
  }

  test("trust:plain-dep:java") {
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

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain' and dependency using Java")
    }
  }

  test("trust:plain-dep:unchecked-cast") {
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

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain' and dependency using unchecked cast")
    }
  }

  test("trust:plain-dep:java-unchecked-cast") {
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

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain' and dependency using Java and unchecked cast")
    }
  }

  test("trust:trust-javaclass-dep:plain") {
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
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "trust-javaclass" }
        |""".stripMargin

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'trust-javaclass' and dependency plain")
    } else {
      succeed
    }
  }

  test("trust:trust-javaclass-dep:java") {
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
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "trust-javaclass" }
        |""".stripMargin

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'trust-javaclass' and dependency using Java")
    } else {
      succeed
    }
  }

  test("trust:trust-javaclass-dep:unchecked-cast") {
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
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.0", trust = "trust-javaclass" }
        |""".stripMargin

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'trust-javaclass' and dependency using unchecked cast")
    }
  }

  test("trust:trust-javaclass-dep:java-unchecked-cast") {
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
        |"github:flix/test-pkg-trust-java-unchecked-cast" = { version = "0.1.0", trust = "trust-javaclass" }
        |""".stripMargin

    val (forbidden, message) = checkForbidden(toml, path)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'trust-javaclass' and dependency using Java and unchecked cast")
    }
  }

  /**
    * Returns `true` if a [[SafetyError.Forbidden]] error is found.
    * Always returns all compiler messages in the second entry of the tuple.
    */
  private def checkForbidden(toml: String, path: Path): (Boolean, String) = {
    implicit val out: PrintStream = System.out
    implicit val formatter: Formatter = Formatter.NoFormatter
    val manifest = ManifestParser.parse(toml, null) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val allManifests = FlixPackageManager.findTransitiveDependencies(manifest, path, None) match {
      case Ok(ms) => ms
      case Err(e) => fail(e.message(formatter))
    }

    val pkgs = FlixPackageManager.installAll(allManifests, path, None) match {
      case Ok(ps) => ps
      case Err(e) => fail(e.message(formatter))
    }

    val main: String =
      """
        |pub def main(): Unit \ IO =
        |    TestPkgTrust.entry()
        |""".stripMargin

    val flix = new Flix()
    flix.addSourceCode("Main.flix", main)(SecurityContext.Unrestricted)

    for ((path, trust) <- pkgs) {
      flix.addPkg(path)(SecurityContext.fromTrust(trust))
    }

    val (_, errors) = flix.check()
    val forbidden = errors.exists {
      case _: SafetyError.Forbidden => true
      case _ => false
    }

    (forbidden, flix.mkMessages(errors).mkString(System.lineSeparator()))
  }
}
