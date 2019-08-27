package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.vt.TerminalContext

object CompilerBenchmark {

  val WarmupIterations = 20

  def benchmarkPhases(): Unit = {
    val flix = newFlix();
    for (i <- 0 until WarmupIterations) {
      flix.compile() match {
        case Validation.Success(compilationResult) =>

          // Check if we are in the last iteration.
          if (i == WarmupIterations - 1) {
            val totalLines = compilationResult.getTotalLines()

            // The current time.
            val currentTime = System.currentTimeMillis() / 1000

            // Print timings for each phase.
            for (phase <- flix.phaseTimers) {
              val name = phase.phase
              val phaseTimeNanos = phase.time
              println(s"${name}, ${currentTime}, ${phaseTimeNanos}")
            }
          }

        case Validation.Failure(errors) =>
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
          System.exit(1)
      }
    }
  }

  def benchmarkThroughput(): Unit = {
    val flix = newFlix();
    for (i <- 0 until WarmupIterations) {
      flix.compile() match {
        case Validation.Success(compilationResult) =>

          // Check if we are in the last iteration.
          if (i == WarmupIterations - 1) {
            val currentTime = System.currentTimeMillis() / 1000
            val totalLines = compilationResult.getTotalLines()
            val totalTime = compilationResult.getTotalTime()
            val throughput = totalLines / (totalTime / 1_000_000_000)
            println(s"${currentTime}, ${throughput}")
          }

        case Validation.Failure(errors) =>
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
          System.exit(1)
      }
    }
  }

  /**
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(): Flix = {
    val flix = new Flix()
    flix.setOptions(opts = flix.options.copy(writeClassFiles = false))

    // A subset of test cases. Note: Many test cases are incompatible, hence we can only include some of them.

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLength.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLit.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayLoad.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayNew.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArraySlice.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.ArrayStore.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Arithmetic.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Bitwise.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Comparison.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Binary.Logic.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Block.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.IfThenElse.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Extend.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Literal.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Multiple.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Polymorphism.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Restrict.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Select.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Record.Update.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.Reference.Ref.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLength.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLit.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorLoad.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorNew.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorSlice.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/language/feature/Test.Expression.VectorStore.flix")

    flix.addPath("main/test/ca/uwaterloo/flix/library/TestList.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestMap.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestOption.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestResult.flix")
    flix.addPath("main/test/ca/uwaterloo/flix/library/TestSet.flix")
  }
}
