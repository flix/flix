package ca.uwaterloo.flix.tools

import java.io.PrintWriter

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Options

import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import scala.collection.mutable

/**
  * Evaluates all benchmarks in a model.
  */
object Benchmarker {

  /**
    * The number of times to evaluate the benchmark before measurements.
    */
  val WarmupRounds = 25_000

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  val ActualRounds = 50_000

  /**
    * Evaluates all benchmarks.
    */
  def benchmark(compilationResult: CompilationResult, writer: PrintWriter)(implicit options: Options): Unit = {
    //
    // A mutable list of results. Populated incrementally.
    //
    val results = mutable.ListBuffer.empty[(Symbol.DefnSym, Long)]

    /*
      * Group benchmarks by namespace.
      */
    val benchmarksByNamespace = compilationResult.getBenchmarks.groupBy(_._1.namespace)

    if (!options.json) {
      println("====================== Flix Benchmark ======================")
    }
    /*
     * Iterate through each namespace and evaluate each benchmark.
     */
    for ((ns, benchmarks) <- benchmarksByNamespace) {
      /*
       * Warmup Rounds.
       */
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        for (i <- 0 until WarmupRounds) {
          defn()
        }
      }

      /*
       * Actual Rounds.
       */
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        val totalTime = run(defn, ActualRounds)
        val averageTime = totalTime / ActualRounds
        if (!options.json) {
          writer.println(f"  $sym  $averageTime%,8d ns.")
        }
        results += ((sym, averageTime))
        sleepAndGC()
      }
    }

    if (!options.json) {
      println()
      println(f"Finished ${benchmarksByNamespace.size} benchmarks with $ActualRounds iterations each.")
    }

    // Print JSON
    if (options.json) {
      val json = ("threads" -> options.threads) ~
        ("benchmarks" -> results.toList.map {
          case (sym, time) => ("name" -> sym.toString) ~ ("time" -> time)
        })
      val s = JsonMethods.pretty(JsonMethods.render(json))
      println(s)
    }
  }

  /**
    * Returns the timings of evaluating `f` over `n` rounds.
    */
  private def run(f: () => AnyRef, n: Int): Long = {
    val t = System.nanoTime()
    var i = 0
    var s = 0L
    while (i < n) {
      f()
      i = i + 1
    }
    System.nanoTime() - t
  }

  /**
    * Sleeps for a little while and tries to run the garbage collector.
    */
  private def sleepAndGC(): Unit = {
    Thread.sleep(250)
    System.gc()
    Thread.sleep(250)
  }

}
