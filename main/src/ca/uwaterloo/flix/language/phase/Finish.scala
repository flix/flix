package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Duration, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * Prints some statistics at the end of compilation.
  */
object Finish extends Phase[CompilationResult, CompilationResult] {

  def run(result: CompilationResult)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = {
    // Print throughput.
    if (flix.options.debug) {
      val totalLines = result.getTotalLines()
      val totalTime = result.getTotalTime()
      val timeInSeconds = new Duration(totalTime).seconds
      val linesPerSecond = totalLines.toDouble / timeInSeconds
      Console.println(f"Compiled $totalLines%,d lines in $timeInSeconds%.1f sec. ($linesPerSecond%,.0f lines/sec).")
    }

    // Return the result.
    result.toSuccess
  }

}
