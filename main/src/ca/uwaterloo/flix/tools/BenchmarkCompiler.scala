package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{LocalResource, Options, StatUtils, Validation}
import ca.uwaterloo.flix.util.vt.TerminalContext
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods

/**
  * A collection of internal utilities to measure the performance of the Flix compiler itself.
  */
object BenchmarkCompiler {

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val N = 25

  /**
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    val flix = newFlix(o)

    val results = List.fill(N) {
      flix.compile() match {
        case Validation.Success(_) =>
          flix.phaseTimers.toList
        case Validation.Failure(errors) =>
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
          System.exit(1)
          throw new AssertionError("impossible") // inaccessible
      }
    }

    // phase -> list of times
    val phaseMap = results.flatten.groupMap(_.phase)(_.time)

    // phase -> median time
    val phaseMedians = phaseMap.map {
      case (phase, times) => (phase, StatUtils.median(times))
    }

    for ((phase, time) <- phaseMedians) {
      val currentTime = System.currentTimeMillis() / 1000
      println(s"$phase, $currentTime, $time")
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
    val iteration = timings.indexOf(timings.min)

    // Compute the ration between the slowest and fastest run.
    val ratio = timings.max.toDouble / timings.min.toDouble

    // The number of threads uses.
    val threads = o.threads

    // Print JSON or plain text?
    if (o.json) {
      val json = ("lines" -> lines) ~ ("throughput" -> ("min" -> min) ~ ("max" -> max) ~ ("avg" -> avg) ~ ("median" -> median))
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    } else {
      println("===================== Flix Compiler Throughput =====================")
      println()
      println(f"Throughput (best): $max%,6d lines/sec (with $threads thread(s))")
      println()
      println(f"  min: $min%,6d, max: $max%,6d, avg: $avg%,6d, median: $median%,6d")
      println()
      println(f"  The highest throughput was in: $iteration (out of $N).")
      println(f"  The ratio between the best and worst was: $ratio%1.1fx.")
      println()
      println(f"Completed $N iterations on $lines%,6d lines in $totalTime seconds.")
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
