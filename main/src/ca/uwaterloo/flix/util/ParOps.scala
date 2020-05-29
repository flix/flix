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

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix

import scala.collection.parallel._

object ParOps {

  /**
    * Control the degree of parallelism.
    */
  private val Threads: Int = scala.collection.parallel.availableProcessors

  /**
    * Global fork join pool (per JVM-instance).
    */
  private val forkJoinPool = new java.util.concurrent.ForkJoinPool(Threads)

  /**
    * Global fork join task support (per JVM-instance).
    */
  private val forkJoinTaskSupport = new scala.collection.parallel.ForkJoinTaskSupport(forkJoinPool)

  /**
    * Apply the given function `f` to each element in the list `xs` in parallel.
    */
  @inline
  def parMap[A, B](xs: Iterable[A], f: A => B)(implicit flix: Flix): Iterable[B] = {
    // Build the parallel array.
    val parArray = xs.toParArray

    // Configure the task support.
    parArray.tasksupport = forkJoinTaskSupport

    // Apply the function `f` in parallel.
    val result = parArray.map(f)

    // Return the result as an iterable.
    result.seq
  }

  /**
    * Aggregates the result of applying `seq` and `comb` to `xs`.
    */
  @inline
  def parAgg[A, S](xs: Iterable[A], z: => S)(seq: (S, A) => S, comb: (S, S) => S)(implicit flix: Flix): S = {
    // Build the parallel array.
    val parArray = xs.toParArray

    // Aggregate the result in parallel.
    parArray.aggregate(z)(seq, comb)
  }

}
