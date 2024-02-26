/*
 * Copyright 2020 Magnus Madsen
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


object StatUtils {

  /**
    * Returns the average of `xs`.
    */
  def average[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double = {
    if (xs.isEmpty) return 0.0
    numeric.toDouble(xs.sum) / xs.length.toDouble
  }

  /**
    * Returns the median of `xs`.
    */
  def median[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double = {
    if (xs.isEmpty) throw new IllegalArgumentException("Empty list.")
    if (xs.length == 1) return numeric.toDouble(xs.head)

    val l = xs.sorted
    val n = xs.length
    if (n % 2 == 0) {
      val index = n / 2
      numeric.toDouble(l(index))
    } else {
      val index = n / 2
      (numeric.toDouble(l(index)) + numeric.toDouble(l(index + 1))) / 2
    }
  }

  /**
    * Returns the standard deviation of `xs`.
    */
  def stdDev[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double = {
    val mean = average(xs)
    val deviations = xs.map(mean - numeric.toDouble(_))
    val variance = average(deviations.map { x => x * x })
    Math.sqrt(variance)
  }

}
