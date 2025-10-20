package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.net.URI
import java.nio.file.Files

class TestFlixPackageManager extends AnyFunSuite {
  val s: String = File.separator
  val f: Formatter = Formatter.NoFormatter

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
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val resolution = FlixPackageManager.findTransitiveDependencies(manifest, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(f))
      }
      FlixPackageManager.installAll(resolution, path, None)(Formatter.getDefault, System.out) match {
        case Ok(l) =>
          val (p, _) = l.head
          p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")
        case Err(e) => e.message(f)
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
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val manifests = FlixPackageManager.findTransitiveDependencies(manifest, path, None)(Formatter.getDefault, System.out) match {
        case Ok(resolution) => FlixPackageManager.resolveTrust(resolution)
        case Err(e) => fail(e.message(f))
      }

      FlixPackageManager.installAll(manifests, path, None)(Formatter.getDefault, System.out) match {
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
        case Err(e) => fail(e.message(f)) //should not happen
      }
      val manifest2 = ManifestParser.parse(toml2, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution1 = FlixPackageManager.findTransitiveDependencies(manifest1, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(f))
      }
      val resolution2 = FlixPackageManager.findTransitiveDependencies(manifest2, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(f))
      }
      val resolution = FlixPackageManager.TrustResolution(origin = manifest1, trust = resolution1.trust ++ resolution2.trust, manifestToFlixDeps = resolution1.manifestToFlixDeps ++ resolution2.manifestToFlixDeps)

      FlixPackageManager.installAll(resolution, path, None)(Formatter.getDefault, System.out) match {
        case Ok(l) => l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") } &&
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg") }
        case Err(e) => e.message(f)
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
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")

      val resolution = FlixPackageManager.findTransitiveDependencies(manifest, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
        case Ok(res) => res
        case Err(e) => fail(e.message(f))
      }
      FlixPackageManager.installAll(resolution, path, None)(Formatter.getDefault, System.out) //installs the dependency
      FlixPackageManager.installAll(resolution, path, None)(Formatter.getDefault, System.out) match { //does nothing
        case Ok(l) =>
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") }
        case Err(e) => e.message(f)
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
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.findTransitiveDependencies(manifest, path, None)(Formatter.getDefault, System.out) match {
        case Ok(resolution) => resolution.manifests.contains(manifest) && resolution.manifests.exists(m => m.name == "museum-clerk")
        case Err(e) => e.message(f)
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
      case Err(e) => fail(e.message(f)) //should not happen
    }

    val path = Files.createTempDirectory("")

    FlixPackageManager.findTransitiveDependencies(manifest, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
      case Ok(res) => fail(res.toString)
      case Err(e) =>
        e.message(f)
        succeed
    }
  }

  test("Give error for missing version") {
    assertResult(expected = PackageError.VersionDoesNotExist(SemVer(0, 0, 1), Project("flix", "museum")).message(f))(actual = {
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
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.findTransitiveDependencies(manifest, path, None)(f, System.out).map(FlixPackageManager.resolveTrust) match {
        case Ok(res) => res
        case Err(e) => e.message(f)
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
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      val manifests = FlixPackageManager.findTransitiveDependencies(manifest, path, None)(Formatter.getDefault, System.out) match {
        case Ok(resolution) => FlixPackageManager.resolveTrust(resolution)
        case Err(e) => fail(e.message(f))
      }

      FlixPackageManager.installAll(manifests, path, None)(Formatter.getDefault, System.out) match {
        case Ok(l) =>
          l.exists { case (p, _) => p.endsWith(s"flix${s}museum${s}1.4.0${s}museum-1.4.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-entrance${s}1.2.0${s}museum-entrance-1.2.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg") } &&
            l.exists { case (p, _) => p.endsWith(s"flix${s}museum-restaurant${s}1.1.0${s}museum-restaurant-1.1.0.fpkg") }
        case Err(e) => e.message(f)
      }
    })
  }

}
