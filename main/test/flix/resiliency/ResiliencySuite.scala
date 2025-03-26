package flix.resiliency

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext.AllPermissions
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IteratorHasAsScala

/**
  * Tests invalid Flix files to ensure that resiliency does not cause issues.
  */
class ResiliencySuite extends AnyFunSuite {

  // Run the test for each Flix file in the resiliency test directory.
  for {
    file <- Files.list(Path.of("main/test/flix/resiliency")).iterator().asScala
    if file.toFile.getName.endsWith(".flix")
  } {
    checkFile(file)
  }

  /**
    * Compiles the given file using a fresh Flix instance.
    *
    * The file is expected to have many errors, but the compiler should not crash.
    */
  def checkFile(p: Path): Unit = {
    val name = "Test.Resiliency." + p.getFileName
    test(name) {
      val flix = new Flix()
      flix.addFlix(p)(AllPermissions)
      flix.compile()
    }
  }
}
