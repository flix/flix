package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Formatter
import org.scalatest.funsuite.AnyFunSuite

import java.io.PrintStream
import java.nio.file.Files

class TestTrust extends AnyFunSuite {

  private val Main: String =
    """
      |pub def main(): Unit \ IO =
      |    TestPkgTrust.entry()
      |""".stripMargin


  private val MainTransitive: String =
    """
      |pub def main(): Unit \ IO =
      |    TestPkgTrustTransitive.entry()
      |""".stripMargin


  test("toString-ofString-plain") {
    val perm = Trust.Plain
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
    val deps = List(
      """
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("trust:plain-dep:java") {
    val deps = List(
      """
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain' and dependency using Java")
    }
  }

  test("trust:plain-dep:unchecked-cast") {
    val deps = List(
      """
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain' and dependency using unchecked cast")
    }
  }

  test("trust:unrestricted-dep:plain") {
    val deps = List(
      """
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("trust:unrestricted-dep:java") {
    val deps = List(
      """
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted' and dependency using java")
    } else {
      succeed
    }
  }

  test("trust:unrestricted-dep:unchecked-cast") {
    val deps = List(
      """
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted' and dependency using unchecked cast")
    } else {
      succeed
    }
  }

  test("transitive.trust:plain->unrestricted-dep:java") {
    // expected to fail currently
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain->unrestricted' and dependency using java")
    }
  }

  test("transitive.trust:unrestricted->unrestricted-dep:java") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted->unrestricted' and dependency using java")
    } else {
      succeed
    }
  }

  /**
    * Returns `true` if a [[SafetyError.Forbidden]] error is found.
    * Always returns all compiler messages in the second entry of the tuple.
    */
  private def checkForbidden(deps: List[String], main: String): (Boolean, String) = {
    implicit val out: PrintStream = System.out
    implicit val formatter: Formatter = Formatter.NoFormatter
    val path = Files.createTempDirectory("")
    val toml =
      s"""
         |[package]
         |name = "test"
         |description = "test"
         |version = "0.1.0"
         |flix = "0.65.0"
         |authors = ["flix"]
         |
         |[dependencies]
         |${deps.mkString(System.lineSeparator())}
         |""".stripMargin

    val manifest = ManifestParser.parse(toml, null) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val allManifests = FlixPackageManager.findTransitiveDependencies(manifest, path, None) match {
      case Ok(ms) => ms
      case Err(e: PackageError.TrustError) => return (true, e.toString)
      case Err(e) => fail(e.message(formatter))
    }

    val pkgs = FlixPackageManager.installAll(allManifests, path, None) match {
      case Ok(ps) => ps
      case Err(e) => fail(e.message(formatter))
    }

    val flix = new Flix()
    flix.setOptions(flix.options.copy(checkTrust = true))

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
