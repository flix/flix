package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.tools.pkg.{JarPackageManager, ManifestParser, PackageError}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.Files

class TestJarPackageManager extends AnyFunSuite {
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
          |[jar-dependencies]
          |"junit.jar" = "url:https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter-api/5.3.1/junit-jupiter-api-5.3.1.jar"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      JarPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l.head.endsWith(s"external${s}junit.jar")
        case Err(e) => e.message(f)
      }
    })
  }

  test("Give error for missing dependency") {
    val missingUrl = "https://repo1.maven.org/junit-jupiter-api.jar"
    val missingName = "missing.jar"
    assertResult(expected = PackageError.DownloadErrorJar(missingUrl, missingName, None).message(f))(actual = {
      val toml = {
        s"""
          |[package]
          |name = "test"
          |description = "test"
          |version = "0.0.0"
          |flix = "0.0.0"
          |authors = ["Anna Blume"]
          |
          |[jar-dependencies]
          |"$missingName" = "url:$missingUrl"
          |
          |""".stripMargin
      }

      val manifest = ManifestParser.parse(toml, null) match {
        case Ok(m) => m
        case Err(e) => fail(e.message(f))
      }

      val path = Files.createTempDirectory("")
      JarPackageManager.installAll(List(manifest), path)(System.out) match {
        case Ok(l) => l
        case Err(e) => e.message(f)
      }
    })
  }

}
