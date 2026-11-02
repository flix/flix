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
    *
    * For an odd number of elements this is the middle element of the sorted sequence.
    * For an even number of elements it is the mean of the two middle elements.
    */
  def median[T](xs: Seq[T])(implicit numeric: Numeric[T]): Double = {
    if (xs.isEmpty) throw new IllegalArgumentException("Empty list.")

    val l = xs.sorted
    val n = l.length
    if (n % 2 == 1) {
      numeric.toDouble(l(n / 2))
    } else {
      (numeric.toDouble(l(n / 2 - 1)) + numeric.toDouble(l(n / 2))) / 2
    }
  }

}
