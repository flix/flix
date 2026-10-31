package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.util.{LocalResource, Options}
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods

import java.nio.file.Path

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompilerOld {

  /**
    * Outputs statistics about the size of the generated JVM code.
    */
  def benchmarkCodeSize(o: Options): Unit = {
    val flix = newFlix(o)
    val result = flix.compile().unsafeGet
    val codeSize = result.codeSize

    // Find the number of lines of source code.
    val lines = result.getTotalLines.toLong

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("codeSize" -> codeSize) ~
          ("lines" -> lines)
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Generated Code Size ======================")
      println()
      println(s"Generated ${java.text.NumberFormat.getIntegerInstance.format(codeSize)} Bytes of code from $lines lines of source code.")
    }
  }

  /**
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(o: Options): Flix = {
    // Clear caches
    EffUnification3.Algebra.Cache.clearCaches()

    val flix = new Flix()

    flix.setOptions(opts = o.copy(incremental = false))

    addInputs(flix)

    flix
  }

  /**
    * Adds test code to the benchmarking suite.
    */
  private def addInputs(flix: Flix): Unit = {
    val sctx: SecurityContext = SecurityContext.Unrestricted
    flix.addSource(Path.of("Test.Exp.Fixpoint.PQuery.flix"), LocalResource.get("/test/flix/Test.Exp.Fixpoint.PQuery.flix"), sctx)
    flix.addSource(Path.of("Test.Exp.Fixpoint.PSolve.flix"), LocalResource.get("/test/flix/Test.Exp.Fixpoint.PSolve.flix"), sctx)
  }

}
