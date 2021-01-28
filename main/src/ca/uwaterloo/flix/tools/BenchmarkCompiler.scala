package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.{LocalResource, Options, StatUtils, Validation}
import ca.uwaterloo.flix.util.vt.TerminalContext
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompiler {

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val N = 15

  /**
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    //
    // Collect data from N iterations.
    //
    val r = (0 until N).map {
      case _ =>
        val flix = newFlix(o)
        val compilationResult = flix.compile().get
        (compilationResult, flix.phaseTimers.toList)
    }

    //
    // Split into compilation results and phase results.
    //
    val results = r.map(_._1).toList
    val phases = r.map(_._2)

    // Compute a map from phase -> list of times.
    val phaseTimings = phases.flatten.groupMap(_.phase)(_.time)

    // Compute a map from phase -> minimum time.
    val phaseMinTime = phaseTimings.map {
      case (phase, times) => (phase, times.min)
    }.toList

    // Compute the sum of all the minimum times.
    val totalMinTime = phaseMinTime.map(_._2).sum

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.getTotalLines().toLong

    // Find the timings of each run.
    val timings = results.map(_.getTotalTime())

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Print JSON or plain text?
    if (o.json) {
      val json =
        ("threads" -> threads) ~
          ("lines" -> lines) ~
          ("iterations" -> N) ~
          ("phases" -> phaseMinTime.map {
            case (phase, time) => ("phase" -> phase) ~ ("time" -> time)
          })
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("====================== Flix Compiler Phases ======================")
      println()
      println("Best runtime per phase (low to high):")
      for ((phase, time) <- phaseMinTime.sortBy(_._2)) {
        val msec = time.toDouble / 1_000_000.toDouble
        val percent = 100.0 * (time.toDouble / totalMinTime.toDouble)
        println(f"  $phase%-30s $msec%5.1f ms ($percent%04.1f%%)")
      }
      println()
      println(f"Finished $N iterations on $lines%,6d lines of code in $totalTime seconds.")
    }
  }

  /**
    * Computes the throughput of the compiler.
    */
  def benchmarkThroughput(o: Options): Unit = {
    //
    // Collect data from N iterations.
    //
    val results = (0 until N).map {
      case _ =>
        val flix = newFlix(o)
        flix.compile().get
    }

    // The number of threads used.
    val threads = o.threads

    // Find the number of lines of source code.
    val lines = results.head.getTotalLines().toLong

    // Find the timings of each run.
    val timings = results.map(_.getTotalTime()).toList

    // Compute the total time in seconds.
    val totalTime = (timings.sum / 1_000_000_000L).toInt

    // Find the throughput of each run.
    val throughputs = timings.map(throughput(lines, _))

    // Compute the minimum throughput (per second).
    val min = throughputs.min

    // Compute the maximum throughput (per second).
    val max = throughputs.max

    // Compute the average throughput (per second).
    val avg = StatUtils.avg(throughputs.map(_.toLong)).toInt

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

    flix.setOptions(opts = o.copy(loadClassFiles = false, writeClassFiles = false))

    flix.addInput("TestArray.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestArray.flix"))
    flix.addInput("TestBigInt.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestBigInt.flix"))
    flix.addInput("TestChar.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestChar.flix"))
    flix.addInput("TestFloat32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat32.flix"))
    flix.addInput("TestFloat64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFloat64.flix"))
    flix.addInput("TestFromString.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestFromString.flix"))
    flix.addInput("TestGetOpt.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestGetOpt.flix"))
    flix.addInput("TestHash.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestHash.flix"))
    flix.addInput("TestInt8.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt8.flix"))
    flix.addInput("TestInt16.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt16.flix"))
    flix.addInput("TestInt32.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt32.flix"))
    flix.addInput("TestInt64.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestInt64.flix"))
    flix.addInput("TestList.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestList.flix"))
    flix.addInput("TestMap.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestMap.flix"))
    flix.addInput("TestOption.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestOption.flix"))
    flix.addInput("TestPrelude.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestPrelude.flix"))
    flix.addInput("TestResult.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestResult.flix"))
    flix.addInput("TestSet.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestSet.flix"))
    flix.addInput("TestString.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestString.flix"))
    flix.addInput("TestValidation.flix", LocalResource.get("/test/ca/uwaterloo/flix/library/TestValidation.flix"))

    flix
  }

}
