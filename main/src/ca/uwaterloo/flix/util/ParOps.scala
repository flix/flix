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

import java.util
import java.util.concurrent.{Callable, Executors}

import scala.jdk.CollectionConverters._
import scala.collection.parallel._
import ca.uwaterloo.flix.api.Flix

object ParOps {

  /**
    * Apply the given function `f` to each element in the list `xs` in parallel.
    */
  @inline
  def parMap[A, B](xs: Iterable[A])(f: A => B)(implicit flix: Flix): Iterable[B] = {
    // Build the parallel array.
    val parArray = xs.toParArray

    // Configure the task support.
    parArray.tasksupport = flix.forkJoinTaskSupport

    // Apply the function `f` in parallel.
    val result = parArray.map(f)

    // Return the result as an iterable.
    result.seq
  }

  /**
    * Apply the given function `f` to each value in the map `m` in parallel.
    */
  @inline
  def parMapValues[K, A, B](m: Map[K, A])(f: A => B)(implicit flix: Flix): Map[K, B] =
    parMap(m) {
      case (k, v) => (k, f(v))
    }.toMap

  /**
    * Apply the given fallible function `f` to each value in the map `m` in parallel,
    * returning the resulting map if all calls are successful.
    */
  @inline
  def parMapValuesSeq[K, A, B, E](m: Map[K, A])(f: A => Validation[B, E])(implicit flix: Flix): Validation[Map[K, B], E] = {
    val results = parMap(m) {
      case (k, v) => f(v).map((k, _))
    }
    Validation.sequence(results).map(_.toMap)
  }

  /**
    * Aggregates the result of applying `seq` and `comb` to `xs`.
    */
  @inline
  def parAgg[A, S](xs: Iterable[A], z: => S)(seq: (S, A) => S, comb: (S, S) => S)(implicit flix: Flix): S = {
    // Build the parallel array.
    val parArray = xs.toParArray

    // Configure the task support.
    parArray.tasksupport = flix.forkJoinTaskSupport

    // Aggregate the result in parallel.
    parArray.aggregate(z)(seq, comb)
  }

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  def parReachable[T](init: Set[T], next: T => Set[T])(implicit flix: Flix): Set[T] = {
    // A wrapper for the next function.
    class NextCallable(t: T) extends Callable[Set[T]] {
      override def call(): Set[T] = next(t)
    }

    // Initialize a new executor service.
    val executorService = Executors.newFixedThreadPool(flix.options.threads)

    // A mutable variable that holds the currently reachable Ts.
    var reach = init

    // A mutable variable that holds the reachable Ts discovered in the last iteration.
    var delta = init

    // Iterate until the fixpoint is reached.
    while (delta.nonEmpty) {

      // Construct a collection of callables.
      val callables = new util.ArrayList[NextCallable]
      for (sym <- delta) {
        callables.add(new NextCallable(sym))
      }

      // Invoke all callables in parallel.
      val futures = executorService.invokeAll(callables)

      // Compute the set of all inferred Ts in this iteration.
      // May include Ts discovered in previous iterations.
      val newReach = futures.asScala.foldLeft(Set.empty[T]) {
        case (acc, future) => acc ++ future.get()
      }

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Shutdown the executor service.
    executorService.shutdown()

    // Return the set of reachable Ts.
    reach
  }
}
