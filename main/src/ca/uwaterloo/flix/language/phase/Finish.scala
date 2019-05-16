package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{Duration, Validation, Verbosity}
import ca.uwaterloo.flix.util.Validation._

/**
  * Prints some statistics at the end of compilation.
  */
object Finish extends Phase[CompilationResult, CompilationResult] {

  def run(result: CompilationResult)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = {
    // Compute the total number of lines of code.
    val totalLines = result.getRoot.sources.foldLeft(0) {
      case (acc, (_, sl)) => acc + sl.endLine
    }

    // Compute the total elapsed time.
    val totalTime = flix.phaseTimers.foldLeft(0L) {
      case (acc, phase) => acc + phase.time
    }

    // Print throughput.
    if (flix.options.verbosity == Verbosity.Verbose) {
      val timeInSeconds = new Duration(totalTime).seconds
      val linesPerSecond = totalLines.toDouble / timeInSeconds
      Console.println(f"Compiled $totalLines%,d lines in $timeInSeconds%.1f sec. ($linesPerSecond%,.0f lines/sec).")
    }

    // Return the result.
    result.toSuccess
  }

}
