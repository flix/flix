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

trait Benchmarker {

  /**
    * The number of times to evaluate the benchmark before measurements.
    */
  def WarmupRounds: Int

  /**
    * The number of times to evaluate the benchmark to compute the average.
    */
  def ActualRounds: Int

  /**
    * Returns the timings of evaluating `f` over `n` rounds.
    */
  protected def run(f: () => Any, n: Int): List[Long] = {
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
  protected def median(xs: List[Long]): Long = {
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
  protected def sleepAndGC(): Unit = {
    Thread.sleep(500)
    System.gc()
    Thread.sleep(500)
  }
}
