package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Duration, Validation}

/**
  * Prints some statistics at the end of compilation.
  */
object Finish {

  def run(result: CompilationResult)(implicit flix: Flix): Validation[CompilationResult, CompilationMessage] = {
    val totalLines = result.getTotalLines
    val totalTime = result.totalTime
    val totalBytes = result.codeSize
    val totalClasses = result.totalClasses

    //println("Lines, Time, Bytes, Classes")
    Console.println(f"$totalLines,$totalTime,$totalBytes,$totalClasses")

    result.toSuccess
  }

}
