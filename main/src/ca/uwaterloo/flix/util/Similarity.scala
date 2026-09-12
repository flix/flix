/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util

object Similarity {

  /**
    * Returns the value of the key in `haystacks` which is closets to the given `needle`.
    */
  def closestMatch[V](needle: String, haystacks: Map[String, V]): V = {
    val (_, smallestValue) = haystacks.minBy {
      case (k, _) =>
        if (needle.headOption != k.headOption) {
          // If the needle and key do not start with the same letter then they are infinitely apart.
          1_000_000
        } else {
          // Otherwise, we compute their edit distance.
          levenshtein(needle, k)
        }
    }
    smallestValue
  }

  /**
    * Returns the levenshtein distance between s1 and s2.
    *
    * See https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Scala
    */
  private def levenshtein(s1: String, s2: String): Int = {
    val lenStr1 = s1.length
    val lenStr2 = s2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1

      d(i)(j) = min(
        d(i-1)(j  ) + 1,     // deletion
        d(i  )(j-1) + 1,     // insertion
        d(i-1)(j-1) + cost   // substitution
      )
    }

    d(lenStr1)(lenStr2)
  }

  /**
    * Returns the minimum of the given `nums`.
    */
  private def min(nums: Int*): Int = nums.min

}
