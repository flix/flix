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

import java.util.concurrent.{Callable, Executors}

import scala.jdk.CollectionConverters._
import scala.collection.parallel._
import ca.uwaterloo.flix.api.Flix

import java.util.concurrent.ExecutorService

object ParOps {

  /**
    * Apply the given function `f` to each element in the list `xs` in parallel.
    */
  @inline
  def parMap[A, B](xs: Iterable[A])(f: A => B)(implicit flix: Flix): Iterable[B] = {
    val in: Vector[A] = xs.toVector
    val out: scala.collection.mutable.ArrayBuffer[B] = scala.collection.mutable.ArrayBuffer.fill(in.size)(null.asInstanceOf[B])

    val executorService: java.util.concurrent.ExecutorService = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
    try {
      for ((elm, idx) <- xs.zipWithIndex) {
        executorService.submit(new Runnable {
          override def run(): Unit = {
            out(idx) = f(elm)
          }
        }
        )
      }
    } finally {
      executorService.close()
    }

    out
  }

  /**
    * Apply the given function `f` to each value in the map `m` in parallel.
    */
  @inline
  def mapValues[K, A, B](m: Map[K, A])(f: A => B)(implicit flix: Flix): Map[K, B] =
    parMap(m) {
      case (k, v) => (k, f(v))
    }.toMap

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
      val callables = new java.util.ArrayList[NextCallable]
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
