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
import java.util.concurrent.{Callable, CountDownLatch, Future}
import scala.jdk.CollectionConverters._
import ca.uwaterloo.flix.api.Flix

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object ParOps {

  /**
    * Apply the given function `f` to each element in the list `xs` in parallel.
    */
  @inline
  def parMap[A, B](xs: Iterable[A])(f: A => B)(implicit flix: Flix): Iterable[B] = {
    val size = xs.size
    val out = ArrayBuffer.fill(size)(null.asInstanceOf[B])

    val threadPool = flix.threadPool
    val latch = new CountDownLatch(size)
    for ((elm, idx) <- xs.zipWithIndex) {
      threadPool.submit(new Runnable {
        override def run(): Unit = {
          out(idx) = f(elm)
          latch.countDown()
        }
      })
    }

    // Await all
    latch.await()

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
    val size = xs.size
    val threads = flix.options.threads
    val threadPool = flix.threadPool

    // Distribute elements about equally between the threads
    val threadElms = Array.fill(threads) {
      val elmsBuilder = ArrayBuffer.newBuilder[A]
      elmsBuilder.sizeHint(size / threads)
      elmsBuilder.result()
    }
    val iterator = xs.iterator
    val (l1, l2) = threadElms.splitAt(threads - (size % threads))
    l1.foreach {
      l => l.appendAll(iterator.take(size / threads))
    }
    l2.foreach {
      l => l.appendAll(iterator.take(size / threads + 1))
    }

    // Combing is done one after the other,
    // as in task 2 combs the result of 1 and 2,
    // task 3 combs the result of 2 and 3 and so on.
    //
    // This is found to be slightly faster than doing it sequentially,
    // but it could be improved to be more parallel.
    val futuresBuilder = ArrayBuffer.newBuilder[Future[S]]
    futuresBuilder.sizeHint(threads)
    val futures = futuresBuilder.result()
    for ((elms, idx) <- threadElms.zipWithIndex) {
      futures += threadPool.submit(() => {
        val result = elms.foldLeft(z)(seq)
        if (idx > 0)
          comb(futures(idx - 1).get(), result)
        else
          result
      })
    }

    // Assume that threads isn't 0
    futures.last.get()
  }

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  def parReachable[T](init: Set[T], next: T => Set[T])(implicit flix: Flix): Set[T] = {
    // A wrapper for the next function.
    class NextCallable(t: T) extends Callable[Set[T]] {
      override def call(): Set[T] = next(t)
    }

    // Use global thread pool.
    val threadPool = flix.threadPool

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
      val futures = threadPool.invokeAll(callables)

      // Compute the set of all inferred Ts in this iteration.
      // May include Ts discovered in previous iterations.
      val newReach = futures.asScala.foldLeft(Set.empty[T]) {
        case (acc, future) => acc ++ future.get()
      }

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Return the set of reachable Ts.
    reach
  }
}
