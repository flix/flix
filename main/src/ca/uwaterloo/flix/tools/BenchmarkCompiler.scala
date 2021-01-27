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
    * The number of compilations to perform before the statistics are collected.
    */
  val WarmupIterations = 1

  /**
    * The number of compilations to perform when collecting statistics.
    */
  val BenchmarkIterations = 2

  /**
    * Outputs statistics about time spent in each compiler phase.
    */
  def benchmarkPhases(o: Options): Unit = {
    val flix = newFlix(o)

    warmup(flix)

    val results = List.fill(BenchmarkIterations) {
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
    val flix = newFlix(o)

    // Warmup
    warmup(flix)

    // Measure
    val results = (0 until BenchmarkIterations).map {
      case _ => flix.compile().get
    }

    // Computes the throughput in lines/sec.
    val totalLines = results.head.getTotalLines().toLong
    val totalTime = StatUtils.median(results.map(_.getTotalTime()).toList)
    val currentTime = System.currentTimeMillis() / 1000
    val throughput = (1_000_000_000L * totalLines) / totalTime // NB: Careful with loss of precision.

    if (o.json) {
      val json = ("totalLines" -> totalLines) ~ ("throughput" -> throughput)
      println(JsonMethods.pretty(JsonMethods.render(json)))
    } else {
      println(s"$currentTime, $throughput")
    }

  }

  /**
    * Runs the compiler a number of times to warmup the JIT.
    */
  private def warmup(flix: Flix): Unit = {
    for (i <- 0 until WarmupIterations) {
      flix.compile() match {
        case Validation.Success(compilationResult) => // nop
        case Validation.Failure(errors) =>
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt(TerminalContext.AnsiTerminal)))
          System.exit(1)
      }
    }
  }

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
