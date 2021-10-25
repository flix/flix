package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.util.{Formatter, Options}
import org.scalatest.FunSuite

import java.nio.file.Files

class TestPackager extends FunSuite {

  private implicit val terminalContext: Formatter = Formatter.NoFormatter

  private val ProjectPrefix: String = "flix-project-"

  private val DefaultOptions: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
  }

  test("check") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.check(p, DefaultOptions)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.build(p, DefaultOptions)
  }

  test("build-jar") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.build(p, DefaultOptions)
    Packager.buildJar(p, DefaultOptions)
  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.buildPkg(p, DefaultOptions)
  }

  test("benchmark") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.benchmark(p, DefaultOptions)
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.run(p, DefaultOptions)
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    Packager.init(p, DefaultOptions)
    Packager.test(p, DefaultOptions)
  }

}
