/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

import org.scalatest.funsuite.AnyFunSuite

class TestStatUtils extends AnyFunSuite {

  test("minimum.Empty") {
    assert(StatUtils.minimum(Seq.empty[Int]) == 0)
  }

  test("minimum.NonEmpty") {
    assert(StatUtils.minimum(Seq(3L, 1L, 2L)) == 1L)
  }

  test("average.Empty") {
    assert(StatUtils.average(Seq.empty[Int]) == 0.0)
  }

  test("average.NonEmpty") {
    assert(StatUtils.average(Seq(1, 2, 3, 4)) == 2.5)
  }

  test("median.Empty") {
    assertThrows[IllegalArgumentException](StatUtils.median(Seq.empty[Int]))
  }

  test("median.Single") {
    assert(StatUtils.median(Seq(7)) == 7.0)
  }

  test("median.Odd.01") {
    assert(StatUtils.median(Seq(1, 2, 3)) == 2.0)
  }

  test("median.Odd.02") {
    // The middle element, not the mean of the middle element and its successor.
    assert(StatUtils.median(Seq(1, 2, 3, 4, 100)) == 3.0)
  }

  test("median.Even.01") {
    assert(StatUtils.median(Seq(1, 2)) == 1.5)
  }

  test("median.Even.02") {
    // The mean of the two middle elements, not the upper one.
    assert(StatUtils.median(Seq(1, 2, 3, 100)) == 2.5)
  }

  test("median.Unsorted") {
    assert(StatUtils.median(Seq(9, 1, 5, 3, 7)) == 5.0)
    assert(StatUtils.median(Seq(9L, 1L, 5L, 3L)) == 4.0)
  }

  test("median.Double") {
    assert(StatUtils.median(Seq(0.5, 1.5, 2.5, 3.5)) == 2.0)
  }

}
