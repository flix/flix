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

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object ParOps {

  /**
    * Apply the given function `f` to each element in the list `xs` in parallel.
    */
  def parMap[A, B](f: A => B, xs: List[A])(implicit flix: Flix): List[B] = {
    // Retrieve the execution context.
    implicit val _ = flix.ec

    // Apply the function `f` in parallel.
    val futures = xs.map(x => Future(f(x)))

    // Wait for all the results.
    Await.result(Future.sequence(futures), Duration.Inf)
  }

}
