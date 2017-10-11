/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.runtime

import java.io.PrintWriter

/**
  * Evaluates all benchmarks in a model.
  */
object Benchmarker {

  /**
    * The number of times to evaluate the benchmark before measurements.
    */
  val WarmupRounds = 100

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  val ActualRounds = 50

  /**
    * Evaluates all benchmarks in the given `model`.
    */
  def benchmark(model: Model, writer: PrintWriter): Unit = {
    /*
      * Group benchmarks by namespace.
      */
    val benchmarksByNamespace = model.getBenchmarks.groupBy(_._1.namespace)

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
      writer.println(s"      Name:                Median (ms)")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        val timings = run(defn, ActualRounds)
        val medianInMiliSeconds = median(timings).toDouble / (1000.0 * 1000.0)
        writer.println(f"      ${sym.name} $medianInMiliSeconds%20.1f")
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
