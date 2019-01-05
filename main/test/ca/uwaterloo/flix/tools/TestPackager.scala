package ca.uwaterloo.flix.tools

import java.nio.file.Files

import ca.uwaterloo.flix.tools
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestPackager extends FunSuite {

  val Opts: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory("flix-project-")
    tools.Packager.init(p, Opts)
  }

  test("build") {
    val p = Files.createTempDirectory("flix-project-")
    tools.Packager.init(p, Opts)
    tools.Packager.build(p, Opts)
  }

  //  ignore("build-jar") {
  //    val p = Files.createTempDirectory("flix-project-")
  //    PackageManager.init(p, Opts)
  //    PackageManager.build(p, Opts)
  //    PackageManager.buildJar(p, Opts)
  //  }

  test("build-pkg") {
    val p = Files.createTempDirectory("flix-project-")
    tools.Packager.init(p, Opts)
    tools.Packager.buildPkg(p, Opts)
  }

  test("run") {
    val p = Files.createTempDirectory("flix-project-")
    tools.Packager.init(p, Opts)
    tools.Packager.run(p, Opts)
  }

  test("test") {
    val p = Files.createTempDirectory("flix-project-")
    tools.Packager.init(p, Opts)
    tools.Packager.test(p, Opts)
  }

}
