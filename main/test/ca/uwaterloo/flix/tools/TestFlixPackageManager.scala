package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, ManifestParser, PackageError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

import java.nio.file.Files

class TestFlixPackageManager extends FunSuite {

  test("Install missing dependency.01") {
    assertResult(expected = Ok(List("magnus-madsen/helloworld/ver1.0.0/helloworld.fpkg")))(actual = {
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
      FlixPackageManager.installAll(manifest, path)(System.out)
    })
  }

  test("Install missing dependency.02") {
    assertResult(expected = Ok(List("magnus-madsen/helloworld/ver1.0.0/helloworld.fpkg",
                                    "magnus-madsen/helloworld/ver1.1.0/helloworld.fpkg")))(actual = {
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
      FlixPackageManager.installAll(manifest, path)(System.out)
    })
  }

  test("Do not install existing dependency") {
    assertResult(expected = Ok(List()))(actual = {
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
      FlixPackageManager.installAll(manifest, path)(System.out) //does nothing
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
