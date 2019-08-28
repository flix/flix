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
  val WarmupRounds = 25000

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  val ActualRounds = 25000

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
      if (ns.isEmpty) {
        writer.println(s"-- Benchmarks for root -- ")
      } else {
        writer.println(s"-- Benchmarks for '${ns.mkString(".")}' -- ")
      }

      /*
       * Warmup Rounds.
       */
      writer.println(s"    Warmup Rounds: $WarmupRounds")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        writer.print("      ")
        writer.print(sym.name)
        writer.print(": ")
        for (i <- 0 until WarmupRounds) {
          writer.print(".")
          defn()
        }
        writer.println()
      }
      writer.println()

      /*
       * Actual Rounds.
       */
      writer.println(s"    Actual Rounds: $ActualRounds")
      writer.println(s"      Name:                Median (us)")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        val timings = run(defn, ActualRounds)
        val medianInMicroSeconds = median(timings).toDouble / (1000.0)
        writer.println(f"      ${sym.name} $medianInMicroSeconds%20.1f")
        sleepAndGC()
      }

      writer.println()
    }
  }

  /**
    * Returns the timings of evaluating `f` over `n` rounds.
    */
  private def run(f: () => AnyRef, n: Int): List[Long] = {
    var result = List.empty[Long]
    var i = 0
    while (i < n) {
      val t = System.nanoTime()
      f()
      val e = System.nanoTime() - t
      i = i + 1
      result = e :: result
    }
    result
  }

  /**
    * Returns the median of the given list of longs.
    */
  private def median(xs: List[Long]): Long = {
    if (xs.isEmpty) throw new IllegalArgumentException("Empty list.")
    if (xs.length == 1) return xs.head

    val l = xs.sorted
    val n = xs.length
    if (n % 2 == 0) {
      val index = n / 2
      l(index)
    } else {
      val index = n / 2
      (l(index) + l(index + 1)) / 2
    }
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
