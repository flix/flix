package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintStream}
import java.nio.file.Files

class TestFlixPackageManager extends AnyFunSuite with BeforeAndAfter {
  private val s: String = File.separator
  private implicit val formatter: Formatter = Formatter.NoFormatter
  private implicit val out: PrintStream = System.out

  before { // before each test, sleep for 1000 ms
    Thread.sleep(1000)
  }

  private def throttle[A](action: => A): A = {
    Thread.sleep(1500)
    val result = action
    Thread.sleep(1500)
    result
  }

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
      val resolution = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None).map(FlixPackageManager.resolveTrust) match {
          case Ok(res) => res
          case Err(e) => fail(e.message(formatter))
        }
      }
      throttle {
        FlixPackageManager.installAll(resolution, path, None)
      } match {
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
      val manifests = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None) match {
          case Ok(resolution) => FlixPackageManager.resolveTrust(resolution)
          case Err(e) => fail(e.message(formatter))
        }
      }
      throttle {
        FlixPackageManager.installAll(manifests, path, None)
      } match {
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

      val resolution1 = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest1, path, None).map(FlixPackageManager.resolveTrust) match {
          case Ok(res) => res
          case Err(e) => fail(e.message(formatter))
        }
      }
      val resolution2 = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest2, path, None).map(FlixPackageManager.resolveTrust) match {
          case Ok(res) => res
          case Err(e) => fail(e.message(formatter))
        }
      }
      val resolution = FlixPackageManager.TrustResolution(origin = manifest1, trust = resolution1.trust ++ resolution2.trust, manifestToFlixDeps = resolution1.manifestToFlixDeps ++ resolution2.manifestToFlixDeps)

      throttle {
        FlixPackageManager.installAll(resolution, path, None)
      } match {
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

      val resolution = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None).map(FlixPackageManager.resolveTrust) match {
          case Ok(res) => res
          case Err(e) => fail(e.message(formatter))
        }
      }
      throttle {
        FlixPackageManager.installAll(resolution, path, None) //installs the dependency
      }
      throttle {
        FlixPackageManager.installAll(resolution, path, None) //does nothing
      } match {
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
      throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None)
      } match {
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
    throttle {
      FlixPackageManager.findTransitiveDependencies(manifest, path, None).map(FlixPackageManager.resolveTrust)
    } match {
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
      throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None).map(FlixPackageManager.resolveTrust)
      } match {
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

      val manifests = throttle {
        FlixPackageManager.findTransitiveDependencies(manifest, path, None) match {
          case Ok(resolution) => FlixPackageManager.resolveTrust(resolution)
          case Err(e) => fail(e.message(formatter))
        }
      }
      throttle {
        FlixPackageManager.installAll(manifests, path, None)
      } match {
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

  ignore("trust:plain-dep:plain") {
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

  ignore("trust:unrestricted-dep:plain") {
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

  ignore("trust:unrestricted-dep:java") {
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

  ignore("trust:unrestricted-dep:unchecked-cast") {
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

  ignore("transitive.trust:plain->plain-dep:plain") {
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

  ignore("transitive.trust:unrestricted->plain-dep:plain") {
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

  ignore("transitive.diamond.trust:plain->plain+plain-dep:plain") {
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

  ignore("transitive.diamond.trust:plain->plain+unrestricted-dep:plain") {
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

  ignore("transitive.diamond.trust:unrestricted->plain+plain-dep:plain") {
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

  ignore("transitive.diamond.trust:unrestricted->plain+unrestricted-dep:plain") {
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

  ignore("transitive.trust:unrestricted->unrestricted-dep:java") {
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

  ignore("transitive.diamond.trust:unrestricted->unrestricted+unrestricted-dep:java") {
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

    val allManifests = throttle {
      FlixPackageManager.findTransitiveDependencies(manifest, path, None) match {
        case Ok(ms) => ms
        case Err(e) => fail(e.message(formatter))
      }
    }

    val manifestsWithTrust = FlixPackageManager.resolveTrust(allManifests)

    val trustResolutionErrors = FlixPackageManager.checkTrust(manifestsWithTrust)
    if (trustResolutionErrors.nonEmpty) {
      return (true, trustResolutionErrors.mkString(System.lineSeparator()))
    }

    val pkgs = throttle {
      FlixPackageManager.installAll(manifestsWithTrust, path, None) match {
        case Ok(ps) => ps
        case Err(e) => fail(e.message(formatter))
      }
    }

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
