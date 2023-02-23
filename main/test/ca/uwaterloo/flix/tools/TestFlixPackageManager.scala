package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, ManifestParser, PackageError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

import java.io.File
import java.nio.file.{Files, Paths}

class TestFlixPackageManager extends FunSuite {
  val s = File.separator

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
          |"github:magnus-madsen/helloworld" = "1.0.0"
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
        case Err(_) => ??? //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(manifest, path)(System.out) match {
        case Ok(l) => l.head.endsWith(s"magnus-madsen${s}helloworld${s}ver1.0.0${s}helloworld.fpkg")
        case Err(e) => e
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
          |"github:magnus-madsen/helloworld" = "1.0.0"
          |
          |[dev-dependencies]
          |"github:magnus-madsen/helloworld" = "1.1.0"
          |
          |[mvn-dependencies]
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(_) => ??? //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(manifest, path)(System.out) match {
        case Ok(l) => l.head.endsWith(s"magnus-madsen${s}helloworld${s}ver1.0.0${s}helloworld.fpkg") &&
                      l(1).endsWith(s"magnus-madsen${s}helloworld${s}ver1.1.0${s}helloworld.fpkg")
        case Err(e) => e
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
          |"github:magnus-madsen/helloworld" = "1.0.0"
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
        case Err(_) => ??? //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(manifest, path)(System.out) //installs the dependency
      FlixPackageManager.installAll(manifest, path)(System.out) match { //does nothing
        case Ok(l) => l.head.endsWith(s"magnus-madsen${s}helloworld${s}ver1.0.0${s}helloworld.fpkg")
        case Err(e) => e
      }
    })
  }

  test("Give error for missing dependency") {
    assertResult(expected = Err(PackageError.ProjectNotFound("Could not open stream to https://api.github.com/repos/AnnaBlume99/helloworld/releases")))(actual = {
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
        case Err(_) => ??? //should not happen
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(manifest, path)(System.out)
    })
  }

  test("Give error for missing version") {
    assertResult(expected = Err(PackageError.VersionDoesNotExist("Version 0.0.1 of project magnus-madsen/helloworld does not exist")))(actual = {
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
          |"github:magnus-madsen/helloworld" = "0.0.1"
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
        case Err(_) => ???
      }

      val path = Files.createTempDirectory("")
      FlixPackageManager.installAll(manifest, path)(System.out)
    })
  }

}
