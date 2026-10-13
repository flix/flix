package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.DoNotDiscover
import ca.uwaterloo.flix.tools.pkg.PkgTestUtils.ManifestPath
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.net.URI
import java.nio.file.Files

@DoNotDiscover
class TestJarPackageManager extends AnyFunSuite {
  val s: String = File.separator
  val f: Formatter = Formatter.NoFormatter

  test("Install dependency") {
    assertResult(expected = true)(actual = {
      val toml = {
        """
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[jar-dependencies]
          |"junit.jar" = "url:https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter-api/5.3.1/junit-jupiter-api-5.3.1.jar"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      JarPackageManager.installAll(List(manifest), path, None)(System.out) match {
        case Ok(l) => l.head.endsWith(s"external${s}junit.jar")
        case Err(e) => e.message(f)
      }
    })
  }

  test("Give error for missing dependency") {
    val missingUrl = "https://repo1.maven.org/junit-jupiter-api.jar"
    val missingName = "missing.jar"
    // A jar that is not there is reported as the status the server answered with, rather than
    // as a file that failed to appear.
    val expected = PackageError.DownloadFailed(new URI(missingUrl).toURL, 404).message(f)
    assertResult(expected)(actual = {
      val toml = {
        s"""
          |[package]
          |version = "0.0.0"
          |flix = "0.0.0"
          |
          |[jar-dependencies]
          |"$missingName" = "url:$missingUrl"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, ManifestPath) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      JarPackageManager.installAll(List(manifest), path, None)(System.out) match {
        case Ok(l) => l
        case Err(e) => e.message(f)
      }
    })
  }

}
