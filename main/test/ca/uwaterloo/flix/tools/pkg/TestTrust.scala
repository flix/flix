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

  test("lub.01") {
    assertResult(Trust.Plain)(Trust.Plain.lub(Trust.Plain))
  }

  test("lub.02") {
    assertResult(Trust.Unrestricted)(Trust.Plain.lub(Trust.Unrestricted))
  }

  test("lub.03") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Plain))
  }

  test("lub.04") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.lub(Trust.Unrestricted))
  }

  test("glb.01") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Plain))
  }

  test("glb.02") {
    assertResult(Trust.Plain)(Trust.Plain.glb(Trust.Unrestricted))
  }

  test("glb.03") {
    assertResult(Trust.Plain)(Trust.Unrestricted.glb(Trust.Plain))
  }

  test("glb.04") {
    assertResult(Trust.Unrestricted)(Trust.Unrestricted.glb(Trust.Unrestricted))
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

  test("transitive.trust:plain->plain-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'plain->plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.trust:unrestricted->plain-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted->plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.trust:plain->plain+plain-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "plain" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'plain->plain+plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.trust:plain->plain+unrestricted-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "plain" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'plain->plain+unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.trust:unrestricted->plain+plain-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "unrestricted" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted->plain+plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.trust:unrestricted->plain+unrestricted-dep:plain") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.0", trust = "unrestricted" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted->plain+unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.trust:plain->unrestricted-dep:java") {
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

  test("transitive.diamond.trust:plain->unrestricted+plain-dep:java") {
    // Dep `jaschdoc/flix-test-pkg-trust-transitive-java` depends on `flix/test-pkg-trust-java` with unrestricted trust.
    // Here `flix/test-pkg-trust-java` is depended upon with plain trust.
    // This should result in an error, since `flix/test-pkg-trust-java` uses java
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "plain" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain->unrestricted+plain' and dependency using java")
    }
  }

  test("transitive.diamond.trust:plain->unrestricted+unrestricted-dep:java") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "plain" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'plain->unrestricted+unrestricted' and dependency using java")
    }
  }

  test("transitive.diamond.trust:unrestricted->unrestricted+plain-dep:java") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "plain" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with trust 'unrestricted->unrestricted+plain' and dependency using java")
    }
  }

  test("transitive.diamond.trust:unrestricted->unrestricted+unrestricted-dep:java") {
    val deps = List(
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.0", trust = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", trust = "unrestricted" }
        |""".stripMargin
    )
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with trust 'unrestricted->unrestricted+unrestricted' and dependency using java")
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
      case Err(e) => fail(e.message(formatter))
    }

    val manifestsWithTrust = FlixPackageManager.resolveTrust(allManifests)

    val trustResolutionErrors = FlixPackageManager.checkTrust(manifestsWithTrust)
    if (trustResolutionErrors.nonEmpty) {
      return (true, trustResolutionErrors.mkString(System.lineSeparator()))
    }

    val pkgs = FlixPackageManager.installAll(manifestsWithTrust, path, None) match {
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
