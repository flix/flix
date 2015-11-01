package ca.uwaterloo.flix.util.misc

object Levenshtein {

  val Threshold: Int = 5

  def bestMatch(needle: String, haystack: Traversable[String]): Option[String] = {
    // compute the levenshtein distance for each string in the haystack.
    val m = haystack.toList.map {
      case candidate => (levenshtein(needle, candidate), candidate)
    }

    // find the candidate with the smallest distance.
    val bestCandidate = m.sortBy(_._1).headOption
    bestCandidate flatMap {
      case (distance, candidate) if distance > Threshold => None
      case (distance, candidate) => Some(candidate)
    }
  }

  /**
   * Returns the Levenshtein distance between `s1` and `s2`.
   *
   * Courtesy of https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Scala
   */
  // TODO: Verify that we can use this under the license we plan to use.
  def levenshtein(s1: String, s2: String): Int = {
    val lenStr1 = s1.length
    val lenStr2 = s2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
      val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1

      d(i)(j) = List(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
      ).min
    }

    d(lenStr1)(lenStr2)
  }
}
