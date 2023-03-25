package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{ManifestParser, MavenPackageManager, PackageError}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.FunSuite
import java.io.File
import java.nio.file.Files

class TestMavenPackageManager extends FunSuite {
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

      val path = Files.createTempDirectory("")
      MavenPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l.head.endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}junit${s}jupiter${s}junit-jupiter-api${s}5.9.2${s}junit-jupiter-api-5.9.2.jar") &&
                      l(1).endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}opentest4j${s}opentest4j${s}1.2.0${s}opentest4j-1.2.0.jar") &&
                      l(2).endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}junit${s}platform${s}junit-platform-commons${s}1.9.2${s}junit-platform-commons-1.9.2.jar") &&
                      l(3).endsWith(s"cache${s}https${s}repo1.maven.org${s}maven2${s}org${s}apiguardian${s}apiguardian-api${s}1.1.2${s}apiguardian-api-1.1.2.jar")
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

      val path = Files.createTempDirectory("")
      MavenPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l
        case Err(e) => e.message(f)
      }
    })
  }

}
