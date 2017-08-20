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

package ca.uwaterloo.flix.runtime

object ScalaBenchmark extends Benchmarker{

  val WarmupRounds: Int = 100

  val ActualRounds: Int = 50

  def main(args: Array[String]): Unit = {
    Console.println(s"-- Benchmarks for List --")
    Console.println(s"    Warmup Rounds: $WarmupRounds")
    for ((defn, ind) <- ScalaListBenchmarks.tests.zipWithIndex) {
      val index = ind + 1
      val suffix = if(index.toString.length == 1) "0".concat(index.toString) else index.toString
      Console.print("      ")
      Console.print(s"benchmark$suffix")
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
    for ((defn, ind) <- ScalaListBenchmarks.tests.zipWithIndex) {
      val index = ind + 1
      val suffix = if(index.toString.length == 1) "0".concat(index.toString) else index.toString
      val timings = run(defn, ActualRounds)
      val medianInMiliSeconds = median(timings).toDouble / (1000.0 * 1000.0)
      Console.println(f"      benchmark$suffix $medianInMiliSeconds%20.1f")
      sleepAndGC()
    }

    Console.println()
  }
}
