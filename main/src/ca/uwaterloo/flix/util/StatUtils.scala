/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util


object StatUtils {

  /**
    * Returns the minimum of `xs`, or `numeric.zero` if empty.
    */
  def minimum[T](xs: Seq[T])(implicit numeric: Numeric[T]): T = {
    if (xs.isEmpty) numeric.zero
    else xs.min
  }

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
