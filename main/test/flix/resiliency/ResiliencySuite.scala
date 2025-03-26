package flix.resiliency

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext.AllPermissions
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IteratorHasAsScala

class ResiliencySuite extends AnyFunSuite {

  for {
    file <- Files.list(Path.of("main", "test", "flix", "resiliency")).iterator().asScala
    _ = println("hello 1")
    if file.toFile.getName.endsWith(".flix")
    _ = println("hello 2")
  } {
    checkFile(file)
  }


  def checkFile(p: Path): Unit = {
    val name = "Test.Resiliency." + p.getFileName
    test(name) {
      val flix = new Flix()
      flix.addFlix(p)(AllPermissions)
      flix.compile()
    }
  }
}
