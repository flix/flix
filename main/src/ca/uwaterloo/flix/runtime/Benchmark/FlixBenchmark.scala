/*
 *  Copyright 2017 Ramin Zarifi
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

package ca.uwaterloo.flix.runtime.Benchmark

import ca.uwaterloo.flix.runtime.Model

object FlixBenchmark extends Benchmarker{

  val WarmupRounds: Int = 100

  val ActualRounds: Int = 50

  /**
    * Evaluates all benchmarks in the given `model`.
    */
  def benchmark(model: Model): Unit = {
    /*
      * Group benchmarks by namespace.
      */
    val benchmarksByNamespace = model.getBenchmarks.groupBy(_._1.namespace)

    /*
     * Iterate through each namespace and evaluate each benchmark.
     */
    for ((ns, benchmarks) <- benchmarksByNamespace) {
      if (ns.isEmpty) {
        Console.println(s"-- Benchmarks for root -- ")
      } else {
        Console.println(s"-- Benchmarks for '${ns.mkString(".")}' -- ")
      }

      /*
       * Warmup Rounds.
       */
      Console.println(s"    Warmup Rounds: $WarmupRounds")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        Console.print("      ")
        Console.print(sym.name)
        Console.print(": ")
        for (i <- 0 until WarmupRounds) {
          Console.print(".")
          defn()
        }
        Console.println()
      }
      Console.println()

      /*
       * Actual Rounds.
       */
      Console.println(s"    Actual Rounds: $ActualRounds")
      Console.println(s"      Name:                Median (ms)")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        val timings = run(defn, ActualRounds)
        val medianInMiliSeconds = median(timings).toDouble / (1000.0 * 1000.0)
        Console.println(f"      ${sym.name} $medianInMiliSeconds%20.1f")
        sleepAndGC()
      }

      Console.println()
    }
  }
}
