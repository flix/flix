/*
 * Copyright 2017 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.util.benchmark

object BenchmarkScala {

  // TODO: Temporary class to be deleted.

  /**
    * The number of times to evaluate the benchmark before measurements.
    */
  val WarmupRounds = 100

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  val ActualRounds = 50


  /**
    * Entry point.
    */
  def main(args: Array[String]): Unit = {

    val bs = List(
      "benchmark01" -> ApplesToOranges.benchmark01 _,
      "benchmark02" -> ApplesToOranges.benchmark02 _,
      "benchmark03" -> ApplesToOranges.benchmark03 _,
      "benchmark04" -> ApplesToOranges.benchmark04 _,
      "benchmark05" -> ApplesToOranges.benchmark05 _,
      "benchmark06" -> ApplesToOranges.benchmark06 _,
      "benchmark07" -> ApplesToOranges.benchmark07 _,
      "benchmark08" -> ApplesToOranges.benchmark08 _,
      "benchmark09" -> ApplesToOranges.benchmark09 _,
      "benchmark10" -> ApplesToOranges.benchmark10 _
    )

    benchmark(bs)

  }

  /**
    * Evaluates all benchmarks.
    */
  def benchmark(benchmarks: List[(String, () => Boolean)]): Unit = {

    /*
     * Warmup Rounds.
     */
    Console.println(s"    Warmup Rounds: $WarmupRounds")
    for ((sym, defn) <- benchmarks) {
      Console.print("      ")
      Console.print(sym)
      Console.print(": ")
      for (i <- 0 until WarmupRounds) {
        Console.print(".")
        assert(defn())
      }
      Console.println()
    }
    Console.println()

    /*
     * Actual Rounds.
     */
    Console.println(s"    Actual Rounds: $ActualRounds")
    Console.println(s"      Name:                Median (ms)")
    for ((sym, defn) <- benchmarks) {
      val timings = run(defn, ActualRounds)
      val medianInMiliSeconds = median(timings).toDouble / (1000.0 * 1000.0)
      Console.println(f"      ${sym} $medianInMiliSeconds%20.1f")
      sleepAndGC()
    }

    Console.println()
  }


  /**
    * Returns the timings of evaluating `f` over `n` rounds.
    */
  private def run(f: () => Boolean, n: Int): List[Long] = {
    var result = List.empty[Long]
    var i = 0
    while (i < n) {
      val t = System.nanoTime()
      assert(f())
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
