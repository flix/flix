package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{Flix, PhaseTime}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.unification.UnificationCache
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{LocalResource, Options, StatUtils}
import org.json4s.JsonDSL.*
import org.json4s.native.JsonMethods

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompilerOld {

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val N = 10

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
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    //
    // Collect data from N iterations.
    //
    val r = (0 until N).map { _ =>
      val flix = newFlix(o)
      val compilationResult = flix.compile().unsafeGet
      (compilationResult, flix.phaseTimers.toList)
    }

    processPhasesResults(o, r)
  }

  def benchmarkIncremental(o: Options): Unit = {
    // Get an initial Flix object and precompile it once.
    val flix = newFlix(o)
    flix.setOptions(flix.options.copy(incremental = true))
    flix.compile()

    //
    // Collect data from N iterations, using the same Flix object.
    //
    val r = (0 until N).map { _ =>
      // Re-add all the inputs to mark them all as changed.
      addInputs(flix)

      val compilationResult = flix.compile().unsafeGet
      (compilationResult, flix.phaseTimers.toList)
    }

    processPhasesResults(o, r)
  }

  /**
    * Processes and prints the results of the phase benchmarking.
    */
  private def processPhasesResults(o: Options, r: IndexedSeq[(CompilationResult, List[PhaseTime])]): Unit = {
    //
    // Split into compilation results and phase results.
    //
    val results = r.map(_._1).toList
    val phases = r.map(_._2)

    // Compute a map from phase -> list of times.
    val phaseTimings = phases.flatten.groupMap(_.phase)(_.time)

    // Compute a map from phase -> statistics.
    val phaseStats = phaseTimings.map {
      case (phase, times) => (phase, SummaryStatistics.from(times))
    }

    // Compute the sum of all the average times.
    val totalMean = phaseStats.values.map(_.mean).sum

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.getTotalLines.toLong

    // Find the timings of each run.
    val timings = results.map(_.totalTime)

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("threads" -> threads) ~
          ("lines" -> lines) ~
          ("iterations" -> N) ~
          ("phases" -> phaseStats.map {
            case (phase, time) => ("phase" -> phase) ~ ("time" -> time.mean)
          })
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Compiler Phases ======================")
      println()
      println("Mean runtime per phase (low to high):")
      for ((phase, time) <- phaseStats.toList.sortBy(_._2.mean)) {
        val msec = time.mean / 1_000_000.toDouble
        val percent = 100.0 * (time.mean / totalMean)
        println(f"  $phase%-30s $msec%5.1f ms ($percent%04.1f%%)")
      }
      println()
      println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")
    }
  }

  /**
    * Computes the throughput of the compiler.
    */
  def benchmarkThroughput(o: Options, frontend: Boolean): Unit = {

    case class Run(lines: Int, time: Long)

    //
    // Collect data from N iterations.
    //
    val results = (0 until N).map { _ =>
      val flix = newFlix(o)

      // Benchmark frontend or entire compiler?
      if (frontend) {
        val root = flix.check().toHardFailure.unsafeGet
        val totalLines = root.sources.foldLeft(0) {
          case (acc, (_, sl)) => acc + sl.endLine
        }
        Run(totalLines, flix.getTotalTime)
      } else {
        val compilationResult = flix.compile().toHardFailure.unsafeGet
        Run(compilationResult.getTotalLines, compilationResult.totalTime)
      }
    }

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.lines.toLong

    // Find the timings of each run.
    val timings = results.map(_.time).toList

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Find the throughput of each run.
    val throughputs = timings.map(throughput(lines, _))

    // Compute the minimum throughput (per second).
    val min = throughputs.min

    // Compute the maximum throughput (per second).
    val max = throughputs.max

    // Compute the average throughput (per second).
    val avg = StatUtils.average(throughputs.map(_.toLong)).toInt

    // Compute the median throughput (per second).
    val median = StatUtils.median(throughputs.map(_.toLong)).toInt

    // Compute the fastest iteration.
    val bestIter = timings.indexOf(timings.min)

    // Compute the ration between the slowest and fastest run.
    val bestWorstRatio = timings.max.toDouble / timings.min.toDouble

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("threads" -> threads) ~
          ("lines" -> lines) ~
          ("iterations" -> N) ~
          ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Compiler Throughput ======================")
      println()
      println(f"Throughput (best): $max%,6d lines/sec (with $threads threads.)")
      println()
      println(f"  min: $min%,6d, max: $max%,6d, avg: $avg%,6d, median: $median%,6d")
      println()
      println(f"  The highest throughput was in iteration: $bestIter (out of $N).")
      println(f"  The ratio between the best and worst iteration was: $bestWorstRatio%1.1fx.")
      println()
      println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")
    }
  }

  /**
    * Returns the throughput per second.
    */
  private def throughput(lines: Long, time: Long): Int = ((1_000_000_000L * lines).toDouble / time.toDouble).toInt

  /**
    * Returns a Flix object configured with the benchmark program.
    */
  private def newFlix(o: Options): Flix = {
    val flix = new Flix()
    flushCaches()

    flix.setOptions(opts = o.copy(incremental = false, loadClassFiles = false))

    addInputs(flix)

    flix
  }

  /**
    * Flushes (clears) all caches.
    */
  private def flushCaches(): Unit = {
    UnificationCache.GlobalBool.clear()
    UnificationCache.GlobalBdd.clear()
  }

  /**
    * Adds test code to the benchmarking suite.
    */
  private def addInputs(flix: Flix): Unit = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    flix.addSourceCode("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addSourceCode("TestChain.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChain.flix"))
    flix.addSourceCode("TestIterator.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestIterator.flix"))
    flix.addSourceCode("TestDelayList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestDelayList.flix"))
    flix.addSourceCode("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addSourceCode("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addSourceCode("TestMutDeque.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutDeque.flix"))
    flix.addSourceCode("TestMutList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutList.flix"))
    flix.addSourceCode("TestMutMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutMap.flix"))
    flix.addSourceCode("TestMutSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMutSet.flix"))
    flix.addSourceCode("TestNel.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestNel.flix"))
    flix.addSourceCode("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addSourceCode("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addSourceCode("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addSourceCode("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addSourceCode("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))
}

  case object SummaryStatistics {
    /**
      * Builds the summary statistics from the given data.
      */
    def from[T](data: Seq[T])(implicit numeric: Numeric[T]): SummaryStatistics = {
      SummaryStatistics(
        min = numeric.toDouble(data.min),
        max = numeric.toDouble(data.max),
        mean = StatUtils.average(data),
        median = StatUtils.median(data),
        stdDev = StatUtils.stdDev(data)
      )
    }
  }

  /**
    * A collection of summary statistics.
    */
  case class SummaryStatistics(min: Double, max: Double, mean: Double, median: Double, stdDev: Double)

}
