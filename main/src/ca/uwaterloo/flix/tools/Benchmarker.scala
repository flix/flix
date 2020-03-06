package ca.uwaterloo.flix.tools

import java.io.PrintWriter

import ca.uwaterloo.flix.runtime.CompilationResult

/**
  * Evaluates all benchmarks in a model.
  */
object Benchmarker {

  /**
    * The number of times to evaluate the benchmark before measurements.
    */
  val WarmupRounds = 10_000

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  val ActualRounds = 100_000

  /**
    * Evaluates all benchmarks.
    */
  def benchmark(compilationResult: CompilationResult, writer: PrintWriter): Unit = {
    /*
      * Group benchmarks by namespace.
      */
    val benchmarksByNamespace = compilationResult.getBenchmarks.groupBy(_._1.namespace)

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
        val averageTimeInNanoSeconds = totalTime / ActualRounds
        writer.println(f"$sym,$averageTimeInNanoSeconds")
        sleepAndGC()
      }

      writer.println()
    }
  }

  /**
    * Returns the timings of evaluating `f` over `n` rounds.
    */
  private def run(f: () => AnyRef, n: Int): Long = {
    var i = 0
    var s = 0L
    while (i < n) {
      val t = System.nanoTime()
      f()
      val e = System.nanoTime() - t
      s = s + e
      i = i + 1
    }
    s
  }

  /**
    * Sleeps for a little while and tries to run the garbage collector.
    */
  private def sleepAndGC(): Unit = {
    Thread.sleep(500)
    System.gc()
    Thread.sleep(500)
  }

}
