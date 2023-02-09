package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{ManifestParser, MavenPackageManager, PackageError}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite

class TestMavenPackageManager extends FunSuite {
  test("Install dependency") {
    assertResult(expected = Ok(()))(actual = {
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
          |
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |"org.junit.jupiter:junit-jupiter-api" = "5.9.2"
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(_) => ??? //should not happen
      }

      MavenPackageManager.installAll(manifest)(System.out)
    })
  }

  test("Give error for missing dependency") {
    assertResult(expected = Err(PackageError.CoursierError("Error in downloading Maven dependency: Error downloading annablume:helloworld:1.2.3")))(actual = {
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
          |
          |[dev-dependencies]
          |
          |[mvn-dependencies]
          |"annablume:helloworld" = "1.2.3"
          |
          |[dev-mvn-dependencies]
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(_) => ??? //should not happen
      }

      MavenPackageManager.installAll(manifest)(System.out)
    })
  }

}
