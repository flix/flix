package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.util.{Formatter, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.{BeforeAndAfter, DoNotDiscover}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, File, InputStream, PrintStream}
import java.net.{URI, URL}
import java.nio.file.{Files, Path}

@DoNotDiscover
class TestFlixPackageManager extends AnyFunSuite with BeforeAndAfter {
  private val s: String = File.separator
  private implicit val formatter: Formatter = Formatter.NoFormatter
  private implicit val out: PrintStream = System.out

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

  test("Install missing dependency.01") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-clerk" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val resolution = FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }

      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken) match {
        case Ok(l) =>
          val (p, _) = l.head
          p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Install missing dependency.02") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val manifests =
        FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken) match {
          case Ok(resolution) => FlixPackageManager.resolveSecurityLevels(resolution)
          case Err(e) => fail(e.message(formatter))
        }
      FlixPackageManager.installAll(manifests, path, PkgTestUtils.gitHubToken) match {
        case Ok(l) => l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") } &&
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg") }
        case Err(e) => e
      }
    })
  }

  test("Install missing dependencies from list of manifests") {
    assertResult(expected = true)(actual = {
      val toml1 = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-clerk" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val toml2 = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest1 = ManifestParser.parse(toml1, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }
      val manifest2 = ManifestParser.parse(toml2, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution1 = FlixPackageManager.findTransitiveDependencies(manifest1, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      val resolution2 = FlixPackageManager.findTransitiveDependencies(manifest2, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      val resolution = FlixPackageManager.SecureResolution(origin = manifest1, security = resolution1.security ++ resolution2.security, manifestToFlixDeps = resolution1.manifestToFlixDeps ++ resolution2.manifestToFlixDeps)


      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken) match {
        case Ok(l) => l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") } &&
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg") }
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Do not install existing dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-giftshop" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution = FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(formatter))
      }
      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken) // installs the dependency
      FlixPackageManager.installAll(resolution, path, PkgTestUtils.gitHubToken) match { // does nothing
        case Ok(l) =>
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") }
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Find transitive dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum-entrance" = "1.2.0"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken) match {
        case Ok(resolution) => resolution.manifests.contains(manifest) && resolution.manifests.exists(m => m.name == "museum-clerk")
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Give error for missing dependency") {
    val toml = {
      """
        |[package]
        |name = "test"
        |description = "test"
        |version = "0.0.0"
        |flix = "0.0.0"
        |authors = ["Anna Blume"]
        |
        |[dependencies]
        |"github:flix/does-not-exist" = "1.0.0"
        |
        |[mvn-dependencies]
        |
        |""".stripMargin
    }

    val manifest = ManifestParser.parse(toml, null) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter)) //should not happen
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
      case Ok(res) => fail(res.toString)
      case Err(e) =>
        e.message(formatter)
        succeed
    }
  }

  test("Give error for missing version") {
    assertResult(expected = PackageError.VersionDoesNotExist(SemVer(0, 0, 1), Project("flix", "museum")).message(formatter))(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum" = "0.0.1"
          |
          |[mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter))
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken).map(FlixPackageManager.resolveSecurityLevels) match {
        case Ok(res) => res
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("Install transitive dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[dependencies]
          |"github:flix/museum" = "1.4.0"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(formatter)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val manifests = FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken) match {
        case Ok(resolution) => FlixPackageManager.resolveSecurityLevels(resolution)
        case Err(e) => fail(e.message(formatter))
      }
      FlixPackageManager.installAll(manifests, path, PkgTestUtils.gitHubToken) match {
        case Ok(l) =>
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum${s}1.4.0${s}museum-1.4.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-entrance${s}1.2.0${s}museum-entrance-1.2.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-restaurant${s}1.1.0${s}museum-restaurant-1.1.0.fpkg") }
        case Err(e) => e.message(formatter)
      }
    })
  }

  test("security:paranoid-dep:plain") {
    val deps =
      """
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "paranoid" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'paranoid' and dependency plain")
    }
  }

  test("security:plain-dep:plain") {
    val deps =
      """
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("security:plain-dep:java") {
    val deps =
      """
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'plain' and dependency using Java")
    }
  }

  test("security:plain-dep:unchecked-cast") {
    val deps =
      """
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'plain' and dependency using unchecked cast")
    }
  }

  test("security:unrestricted-dep:plain") {
    val deps =
      """
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("security:unrestricted-dep:java") {
    val deps =
      """
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted' and dependency using java")
    } else {
      succeed
    }
  }

  test("security:unrestricted-dep:unchecked-cast") {
    val deps =
      """
        |"github:flix/test-pkg-trust-unchecked-cast" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, Main)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted' and dependency using unchecked cast")
    } else {
      succeed
    }
  }

  test("transitive.security:plain->plain-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'plain->plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.security:unrestricted->plain-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted->plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.security:plain->plain+plain-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "plain" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'plain->plain+plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.security:plain->plain+unrestricted-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "plain" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'plain->plain+unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.security:paranoid->plain+plain-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "paranoid" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'paranoid->plain+plain' and dependency plain")
    }
  }

  test("transitive.diamond.security:unrestricted->plain+plain-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted->plain+plain' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.diamond.security:unrestricted->plain+unrestricted-dep:plain") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-plain" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-plain" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted->plain+unrestricted' and dependency plain")
    } else {
      succeed
    }
  }

  test("transitive.security:plain->unrestricted-dep:java") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'plain->unrestricted' and dependency using java")
    }
  }

  test("transitive.security:unrestricted->unrestricted-dep:java") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted->unrestricted' and dependency using java")
    } else {
      succeed
    }
  }

  test("transitive.diamond.security:plain->unrestricted+plain-dep:java") {
    // Dep `jaschdoc/flix-test-pkg-trust-transitive-java` depends on `flix/test-pkg-trust-java` with unrestricted trust.
    // Here `flix/test-pkg-trust-java` is depended upon with plain trust.
    // This should result in an error, since `flix/test-pkg-trust-java` uses java
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "plain" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'plain->unrestricted+plain' and dependency using java")
    }
  }

  test("transitive.diamond.security:plain->unrestricted+unrestricted-dep:java") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "plain" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'plain->unrestricted+unrestricted' and dependency using java")
    }
  }

  test("transitive.diamond.security:unrestricted->unrestricted+plain-dep:java") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "plain" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      succeed
    } else {
      fail(message + System.lineSeparator() + "expected failure with security 'unrestricted->unrestricted+plain' and dependency using java")
    }
  }

  test("transitive.diamond.security:unrestricted->unrestricted+unrestricted-dep:java") {
    val deps =
      """
        |"github:jaschdoc/flix-test-pkg-trust-transitive-java" = { version = "0.1.1", security = "unrestricted" }
        |"github:flix/test-pkg-trust-java" = { version = "0.1.0", security = "unrestricted" }
        |""".stripMargin
    val (forbidden, message) = checkForbidden(deps, MainTransitive)

    if (forbidden) {
      fail(message + System.lineSeparator() + "expected ok with security 'unrestricted->unrestricted+unrestricted' and dependency using java")
    } else {
      succeed
    }
  }

  test("mismatched-versions") {
    val toml = PkgTestUtils.mkTomlWithDeps(
      """
        |"github:jaschdoc/flix-test-pkg-mismatched-versions" = "0.1.0"
        |""".stripMargin
    )
    val manifest = ManifestParser.parse(toml, null) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val path = Files.createTempDirectory("")
    FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken) match {
      case Ok(_) => fail("expected error, got success")
      case Err(_: PackageError.MismatchedVersions) => succeed
      case Err(e) => fail(e.message(formatter))
    }
  }

  /**
    * Returns `true` if a [[SafetyError.Forbidden]] error is found.
    * Always returns all compiler messages in the second entry of the tuple.
    */
  private def checkForbidden(deps: String, main: String): (Boolean, String) = {
    val path = Files.createTempDirectory("")
    val toml = PkgTestUtils.mkTomlWithDeps(deps)

    val manifest = ManifestParser.parse(toml, null) match {
      case Ok(m) => m
      case Err(e) => fail(e.message(formatter))
    }

    val allManifests = FlixPackageManager.findTransitiveDependencies(manifest, path, PkgTestUtils.gitHubToken) match {
      case Ok(ms) => ms
      case Err(e) => fail(e.message(formatter))
    }

    val manifestsWithSecurity = FlixPackageManager.resolveSecurityLevels(allManifests)

    val securityResolutionErrors = FlixPackageManager.checkSecurity(manifestsWithSecurity)
    if (securityResolutionErrors.nonEmpty) {
      return (true, securityResolutionErrors.mkString(System.lineSeparator()))
    }

    val pkgs = FlixPackageManager.installAll(manifestsWithSecurity, path, PkgTestUtils.gitHubToken) match {
      case Ok(ps) => ps
      case Err(e) => fail(e.message(formatter))
    }

    val flix = PkgTestUtils.mkFlix
    flix.addVirtualPath(Path.of("Main.flix"), main)(SecurityContext.Unrestricted)

    for ((path, sctx) <- pkgs) {
      flix.addPkg(path)(sctx)
    }

    val (optRoot, errors) = flix.check()
    val forbidden = errors.exists {
      case _: SafetyError.Forbidden => true
      case _ => false
    }
    (forbidden, CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot))
  }

  // Whether the guess-then-fallback lookup in `FlixPackageManager.tryGuess` falls back decides how
  // many requests a dependency install costs, so it is tested directly, with stubbed results,
  // rather than against GitHub.

  /** A project and version to attribute the stubbed lookups below to. */
  private val StubProject: Project = Project("flix", "museum")
  private val StubVersion: SemVer = SemVer(1, 1, 0)

  /** A host the stubbed lookups below can name without reaching it. */
  private val StubUrl: URL = new URI("https://example.invalid").toURL

  /** The result the lookup falls back to when the guess missed, i.e. the listing lookup. */
  private val Exhausted: Result[InputStream, PackageError] =
    Err(PackageError.NoSuchFile(s"${StubProject.owner}/${StubProject.repo}", "fpkg"))

  /** A stubbed 404: `assetName` is not published by the release. */
  private def notFound(assetName: String): Result[InputStream, PackageError] =
    Err(PackageError.ReleaseAssetNotFound(StubProject, StubVersion, assetName, StubUrl))

  /** A stubbed hit: a stream whose contents are `assetName`, so the winner can be identified. */
  private def found(assetName: String): Result[InputStream, PackageError] =
    Ok(new ByteArrayInputStream(assetName.getBytes))

  /** A stubbed rate limit, i.e. a failure that says nothing about whether the asset exists. */
  private def refused: Result[InputStream, PackageError] =
    Err(PackageError.DownloadRefused(StubUrl, 429, None))

  /** Returns the name of the asset `result` opened, failing if it did not open one. */
  private def openedName(result: Result[InputStream, PackageError]): String = result match {
    case Ok(stream) => new String(stream.readAllBytes())
    case Err(e) => fail(s"expected a stream, got ${e.message(formatter)}")
  }

  test("tryGuess.01: a hit on the guess is used, without falling back") {
    val result = FlixPackageManager.tryGuess(found("museum.fpkg"))(fail("must not fall back after a hit"))
    assertResult(expected = "museum.fpkg")(actual = openedName(result))
  }

  test("tryGuess.02: a 404 on the guess falls back to the listing") {
    val result = FlixPackageManager.tryGuess(notFound("museum.fpkg"))(Exhausted)
    assertResult(expected = Exhausted)(actual = result)
  }

  test("tryGuess.03: a failure other than a 404 is reported without falling back") {
    val result = FlixPackageManager.tryGuess(refused)(fail("must not fall back after a refusal"))
    val reported = result match {
      case Err(e: PackageError.DownloadRefused) => e.status
      case other => fail(s"expected a refusal, got $other")
    }
    assertResult(expected = 429)(actual = reported)
  }

  test("fpkgAssetName.01: the package is guessed under the repository name") {
    val dep = Dependency.FlixDependency(Repository.GitHub, StubProject.owner, StubProject.repo, StubVersion, SecurityContext.Unrestricted)
    assertResult(expected = "museum.fpkg")(actual = FlixPackageManager.fpkgAssetName(dep))
  }

}
