package ca.uwaterloo.flix.tools

import java.nio.file.Files

import ca.uwaterloo.flix.tools
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.util.vt.TerminalContext
import org.scalatest.FunSuite

class TestPackager extends FunSuite {

  private implicit val _ = TerminalContext.NoTerminal

  private val ProjectPrefix: String = "flix-project-"

  private val DefaultOptions: Options = Options.Default

  test("init") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
  }

  test("build") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
    tools.Packager.build(p, DefaultOptions)
  }

  //  ignore("build-jar") {
  //    val p = Files.createTempDirectory("flix-project-")
  //    PackageManager.init(p, Opts)
  //    PackageManager.build(p, Opts)
  //    PackageManager.buildJar(p, Opts)
  //  }

  test("build-pkg") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
    tools.Packager.buildPkg(p, DefaultOptions)
  }

  test("benchmark") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
    tools.Packager.benchmark(p, DefaultOptions)
  }

  test("run") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
    tools.Packager.run(p, DefaultOptions)
  }

  test("test") {
    val p = Files.createTempDirectory(ProjectPrefix)
    tools.Packager.init(p, DefaultOptions)
    tools.Packager.test(p, DefaultOptions)
  }

}
