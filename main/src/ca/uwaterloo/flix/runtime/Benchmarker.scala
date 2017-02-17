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

import ca.uwaterloo.flix.util.Highlight.Magenta

object Benchmarker {

  val WarmupRounds = 25
  val ActualRounds = 25

  def benchmark(model: Model): Unit = {
    /*
      * Group benchmarks by namespace.
      */
    val benchmarksByNamespace = model.getBenchmarks.groupBy(_._1.namespace)

    /*
     * Iterate through each namespace and evaluate tests.
     */
    for ((ns, benchmarks) <- benchmarksByNamespace) {
      if (ns.isEmpty) {
        Console.println(s"-- Benchmarks for ${Magenta("root")} -- ")
      } else {
        Console.println(s"-- Benchmarks for '${Magenta(ns.mkString("."))}' -- ")
      }

      /*
       * Warmup Rounds.
       */
      Console.println(s"    Warmup Rounds: $WarmupRounds")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        // Evaluate the function.
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
      Console.println(s"      Name:                  Time (ms)          Ops (ops/s)")
      for ((sym, defn) <- benchmarks.toList.sortBy(_._1.loc)) {
        val elapsedTimeInNanoSeconds = run(defn, ActualRounds).toDouble
        val meanTimeInMiliSeconds = elapsedTimeInNanoSeconds / (1000 * 1000 * ActualRounds.toDouble)
        val operationsPerSecond = 1000.0 / meanTimeInMiliSeconds
        Console.println(f"      ${sym.name} $meanTimeInMiliSeconds%20.1f $operationsPerSecond%20.1f")
      }

      Console.println()
    }
  }

  /**
    * Returns the elapsed time in nanoseconds of evaluating `f` over `n` rounds.
    */
  @inline
  private def run(f: () => AnyRef, n: Int): Long = {
    val t = System.nanoTime()
    var i = 0
    while (i < n) {
      f()
      i = i + 1
    }
    System.nanoTime() - t
  }

}
