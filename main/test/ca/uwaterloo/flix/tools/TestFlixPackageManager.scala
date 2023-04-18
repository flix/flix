package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, ManifestParser, PackageError, SemVer}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

import java.io.File
import java.net.URL
import java.nio.file.Files

class TestFlixPackageManager extends FunSuite {
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l.head.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")
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
          |[dev-dependencies]
          |"github:flix/museum-clerk" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l.exists(p => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
                      l.exists(p => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg"))
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
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
      FlixPackageManager.installAll(List(manifest1, manifest2), path)(System.out) match {
        case Ok(l) => l.exists(p => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
                      l.exists(p => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg"))
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(List(manifest), path)(System.out) //installs the dependency
      FlixPackageManager.installAll(List(manifest), path)(System.out) match { //does nothing
        case Ok(l) => l.head.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.findTransitiveDependencies(manifest, path)(System.out) match {
        case Ok(l) => l.contains(manifest) && l.exists(m => m.name == "museum-clerk")
        case Err(e) => e.message(f)
      }
    })
  }

  test("Give error for missing dependency") {
    assertResult(expected = PackageError.ProjectNotFound(new URL("https://api.github.com/repos/AnnaBlume99/helloworld/releases"), Project("AnnaBlume99", "helloworld")).message(f))(actual = {
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
          |"github:AnnaBlume99/helloworld" = "1.0.0"
          |
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f)) //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l
        case Err(e) => e.message(f)
      }
    })
  }

  test("Give error for missing version") {
    assertResult(expected = PackageError.VersionDoesNotExist(SemVer(0, 0, Some(1), None, None), Project("flix", "museum")).message(f))(actual = {
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
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l
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
      val manifests = FlixPackageManager.findTransitiveDependencies(manifest, path)(System.out) match {
        case Ok(l) => l
        case Err(e) => fail(e.message(f))
      }
      FlixPackageManager.installAll(manifests, path)(System.out) match {
        case Ok(l) => println(l) //TODO: remove
          l.exists(p => p.endsWith(s"flix${s}museum${s}1.4.0${s}museum-1.4.0.fpkg")) &&
          l.exists(p => p.endsWith(s"flix${s}museum-clerk${s}1.1.0${s}museum-clerk-1.1.0.fpkg")) &&
          l.exists(p => p.endsWith(s"flix${s}museum-entrance${s}1.2.0${s}museum-entrance-1.2.0.fpkg")) &&
          l.exists(p => p.endsWith(s"flix${s}museum-giftshop${s}1.1.0${s}museum-giftshop-1.1.0.fpkg")) &&
          l.exists(p => p.endsWith(s"flix${s}museum-restaurant${s}1.1.0${s}museum-restaurant-1.1.0.fpkg"))
        case Err(e) => e.message(f)
      }
    })
  }

}
