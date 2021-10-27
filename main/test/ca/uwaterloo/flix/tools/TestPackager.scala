package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.util.{Formatter, Options}
import org.scalatest.FunSuite

import java.nio.file.Files

class TestPackager extends FunSuite {

  private val formatter: Formatter = Formatter.NoFormatter

  private val ProjectPrefix: String = "flix-project-"

  private val DefaultOptions: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.check(p, DefaultOptions, formatter)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.build(p, DefaultOptions, formatter)
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.build(p, DefaultOptions, formatter)
    Packager.buildJar(p, DefaultOptions, formatter)
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.buildPkg(p, DefaultOptions, formatter)
  }

  test("benchmark") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.benchmark(p, DefaultOptions, formatter)
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.run(p, DefaultOptions, formatter)
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions, formatter)
    Packager.test(p, DefaultOptions, formatter)
  }

}
