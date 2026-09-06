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
import ca.uwaterloo.flix.util.collection.ListMap

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

/**
  * Parallel versions of common operations — `map`, aggregation, and reachability —
  * that run on the compiler's shared thread pool. Each operation falls back to a sequential
  * implementation when the compiler runs single-threaded.
  *
  * == Scheduling ==
  *
  * All operations are built on [[parFold]], which schedules at most one task per thread and lets
  * the tasks claim work items from a shared counter. This keeps the number of tasks handed to the
  * pool small even for phases with many thousands of tiny work items, and balances the work
  * dynamically across the threads.
  *
  * == Exception Handling ==
  *
  * The user-supplied functions (e.g. `f`, `seq`, `comb`, and `next`) may throw — in the compiler
  * almost always an [[InternalCompilerException]], which the top-level driver in [[Flix]] catches
  * to produce a crash report. Because these functions run on worker threads rather than on the
  * calling thread, [[parFold]] is careful to propagate a thrown exception back to the caller
  * *unchanged*, preserving its original type, message, and source location, so that the crash
  * handler still recognizes it. When several tasks fail concurrently, we rethrow the first
  * exception and attach the rest to it as suppressed exceptions, so that none are silently lost.
  */
object ParOps {

  /**
    * Applies the function `f` to every element of `xs` in parallel.
    */
  def parMap[A, B: ClassTag](xs: Iterable[A])(f: A => B)(implicit flix: Flix): Iterable[B] = {
    // Just map if we're single-threaded.
    if (singleThreaded) {
      return xs.map(f)
    }

    // Copy the input into an array (for O(1) indexing) and construct a new empty array to hold the result.
    val in = xs.toArray[Any]
    val out: Array[B] = new Array(in.length)

    // Compute every element in parallel. The (unit) accumulators are irrelevant.
    parFold(in.length, ())((_, i) => out(i) = f(in(i).asInstanceOf[A]))

    out
  }

  /**
    * Applies the function `f` to every element of `xs` in parallel.
    *
    * The elements are scheduled in ascending `sortBy` order, i.e. the element with the smallest
    * `sortBy` value is started first. Pass e.g. negated sizes to start work early on the biggest
    * tasks and thus increase throughput.
    */
  def parMapWithPriority[A: ClassTag, B: ClassTag](xs: Iterable[A], sortBy: A => Int)(f: A => B)(implicit flix: Flix): Iterable[B] = {
    val arr = xs.toArray
    arr.sortInPlaceBy(sortBy)
    parMap(ArraySeq.unsafeWrapArray(arr))(f)
  }

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    */
  def parMapValues[K, A, B](m: Map[K, A])(f: A => B)(implicit flix: Flix): Map[K, B] =
    parMap(m) {
      case (k, v) => (k, f(v))
    }.toMap

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    *
    * The values are scheduled in ascending `sortBy` order, i.e. the value with the smallest
    * `sortBy` value is started first. Pass e.g. negated sizes to start work early on the biggest
    * tasks and thus increase throughput.
    */
  def parMapValuesWithPriority[K, A, B](m: Map[K, A], sortBy: A => Int)(f: A => B)(implicit flix: Flix): Map[K, B] = {
    val arr = m.toArray
    arr.sortInPlaceBy { case (_, v) => sortBy(v) }
    parMap(ArraySeq.unsafeWrapArray(arr)) {
      case (k, v) => (k, f(v))
    }.toMap
  }

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    *
    * f will be applied to each value in the list.
    */
  def parMapValueList[K, A, B](m: ListMap[K, A])(f: A => B)(implicit flix: Flix): ListMap[K, B] =
    ListMap(
      parMap(m.m) {
        case (k, v) => (k, v.map(f))
      }.toMap
    )

  /**
    * Applies the function `f` to every value of the map `m` in parallel.
    *
    * f will be applied to the list of values.
    */
  def parMapValueList2[K, A, B](m: ListMap[K, A])(f: List[A] => List[B])(implicit flix: Flix): ListMap[K, B] =
    ListMap(
      parMap(m.m) {
        case (k, v) => (k, f(v))
      }.toMap
    )

  /**
    * Aggregates the result of applying `seq` and `comb` to `xs`.
    *
    * Every task folds the elements it claims with `seq` (starting from `z`), and the resulting
    * partial results are then combined with `comb` on the calling thread.
    *
    * == Contract ==
    *
    * The elements are partitioned across the tasks nondeterministically (see [[parFold]]), so the
    * result is only well-defined if:
    *
    *   - `comb` is associative '''and commutative''', and `z` is its neutral element, and
    *   - `seq(comb(s1, s2), x) == comb(s1, seq(s2, x))`, i.e. folding an element into an
    *     accumulator with `seq` agrees with combining it in with `comb`.
    *
    * In particular, the order in which the elements reach `seq` is unspecified. This is a stronger
    * requirement than that of e.g. `Iterable.aggregate`, which preserves the order of the elements
    * and hence only needs `comb` to be associative. It is met by e.g. set union and by merging maps
    * with pairwise disjoint keys, but not by e.g. list concatenation.
    */
  def parAgg[A: ClassTag, S](xs: Iterable[A], z: => S)(seq: (S, A) => S, comb: (S, S) => S)(implicit flix: Flix): S = {
    // Just fold if we're single-threaded.
    if (singleThreaded) {
      return xs.foldLeft(z)(seq)
    }

    // Case 1: The iterable `xs` is empty. We simply return the neutral element z.
    if (xs.isEmpty) {
      return z
    }

    // Case 2: We convert `xs` to an array, fold it in parallel, and combine the partial results.
    // There is at least one partial result since `xs` is non-empty.
    val a = xs.toArray
    parFold(a.length, z)((acc, i) => seq(acc, a(i))).reduce(comb)
  }

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  def parReach[T](init: Set[T], next: T => Set[T])(implicit flix: Flix): Set[T] = {
    if (singleThreaded) {
      return seqReach(init, next)
    }

    // A mutable variable that holds the currently reachable Ts.
    var reach = init

    // A mutable variable that holds the reachable Ts discovered in the last iteration.
    var delta = init

    // Iterate until the fixpoint is reached.
    while (delta.nonEmpty) {
      // Compute the set of all inferred Ts in this iteration.
      // May include Ts discovered in previous iterations.
      val a = delta.toArray[Any]
      val partials = parFold(a.length, Set.empty[T])((acc, i) => acc ++ next(a(i).asInstanceOf[T]))
      val newReach = partials.foldLeft(Set.empty[T])(_ ++ _)

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Return the set of reachable Ts.
    reach
  }

  /**
    * Folds the indices `0 until size` in parallel and returns the partial results.
    *
    * Schedules `min(threads, size)` tasks on the thread pool. Each task starts from its own `z` and
    * repeatedly claims the next unclaimed index from a shared counter and folds it into its
    * accumulator with `step`. Returns the final accumulator of every task (in no particular order).
    *
    * Claiming indices from a shared counter balances the work dynamically across the tasks, and
    * bounds the number of tasks handed to the pool by the number of threads rather than by `size`.
    * The latter keeps the scheduling overhead low even for many tiny work items.
    *
    * The price of dynamic balancing is that '''which''' indices end up in '''which''' accumulator
    * is nondeterministic: it depends on the timing of the tasks. Callers must therefore either
    * treat each index independently (as [[parMap]] does, writing to a distinct slot per index) or
    * fold with an operation whose result does not depend on how the indices are grouped and
    * ordered (as [[parAgg]] and [[parReach]] do; see the contract of [[parAgg]]).
    */
  private def parFold[S](size: Int, z: => S)(step: (S, Int) => S)(implicit flix: Flix): List[S] = {
    // The number of tasks: at most one per thread, and never more than there are indices.
    val tasks = math.min(flix.options.threads, size)
    if (tasks == 0) {
      return Nil
    }

    // The next unclaimed index.
    val next = new AtomicInteger(0)

    // The partial result of every task.
    val partials = new Array[Any](tasks)

    // Construct a new count down latch to track the number of tasks.
    val latch = new CountDownLatch(tasks)

    // Holds the first thrown exception (if any). Any subsequent exceptions are attached to it
    // as suppressed exceptions so that none are silently lost.
    val exception = new AtomicReference[Throwable](null)

    for (t <- 0 until tasks) {
      flix.threadPool.execute(() => {
        try {
          var acc = z
          var i = next.getAndIncrement()
          while (i < size) {
            acc = step(acc, i)
            i = next.getAndIncrement()
          }
          partials(t) = acc
        } catch {
          case ex: Throwable =>
            // Keep the first exception; record any later ones as suppressed.
            if (!exception.compareAndSet(null, ex)) exception.get().addSuppressed(ex)
        } finally {
          latch.countDown()
        }
      })
    }

    // Await all tasks to finish.
    latch.await()

    // Rethrow the first exception (if any).
    val ex = exception.get()
    if (ex != null) throw ex

    partials.toList.asInstanceOf[List[S]]
  }

  /**
    * Returns true if the compiler is running on a single thread.
    */
  private def singleThreaded(implicit flix: Flix): Boolean = flix.options.threads == 1

  /**
    * Computes the set of reachables Ts starting from `init` and using the `next` function.
    */
  private def seqReach[T](init: Set[T], next: T => Set[T]): Set[T] = {
    // A mutable variable that holds the currently reachable Ts.
    var reach = init

    // A mutable variable that holds the reachable Ts discovered in the last iteration.
    var delta = init

    // Iterate until the fixpoint is reached.
    while (delta.nonEmpty) {
      val newReach = delta.flatMap(next)

      // Update delta and reach.
      delta = newReach -- reach
      reach = reach ++ delta
    }

    // Return the set of reachable Ts.
    reach
  }
}
