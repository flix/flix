package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{FlixPackageManager, ManifestParser, MavenPackageManager, PackageError}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Files

class TestMavenPackageManager extends AnyFunSuite {
  val s: String = File.separator
  val f: Formatter = Formatter.NoFormatter

  test("Install dependency") {
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
          |
          |[mvn-dependencies]
          |"org.junit.jupiter:junit-jupiter-api" = "5.9.2"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(_) => ??? //should not happen
      }

      val path = Files.createTempDirectory("")
      MavenPackageManager.installAll(List(manifest), path)(Formatter.getDefault, System.out) match {
        case Ok(l) => l.exists(p => p.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}junit${s}jupiter${s}junit-jupiter-api${s}5.9.2${s}junit-jupiter-api-5.9.2.jar")) &&
                      l.exists(p => p.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}opentest4j${s}opentest4j${s}1.2.0${s}opentest4j-1.2.0.jar")) &&
                      l.exists(p => p.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}junit${s}platform${s}junit-platform-commons${s}1.9.2${s}junit-platform-commons-1.9.2.jar")) &&
                      l.exists(p => p.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}apiguardian${s}apiguardian-api${s}1.1.2${s}apiguardian-api-1.1.2.jar"))
        case Err(e) => e.message(f)
      }
    })
  }

  test("Give error for missing dependency") {
    val coursierError =
      """Error downloading annablume:helloworld:1.2.3""".stripMargin
    assertResult(expected = PackageError.CoursierError(coursierError).message(f))(actual = {
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

      val path = Files.createTempDirectory("")
      MavenPackageManager.installAll(List(manifest), path)(Formatter.getDefault, System.out) match {
        case Ok(l) => l
        case Err(e) => e.message(f)
      }
    })
  }

  test("Install transitive dependency from Flix dependency") {
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
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      val manifests = FlixPackageManager.findTransitiveDependencies(manifest, path, None)(Formatter.getDefault, System.out) match {
        case Ok(l) => l
        case Err(e) => fail(e.message(f))
      }
      MavenPackageManager.installAll(manifests, path)(Formatter.getDefault, System.out) match {
        case Ok(l) => l.head.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}apache${s}commons${s}commons-lang3${s}3.12.0${s}commons-lang3-3.12.0.jar")
        case Err(e) => e.message(f)
      }
    })
  }

}
