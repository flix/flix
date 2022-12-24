package ca.uwaterloo.flix.util

object Grammar {

  /**
    * Returns the ordinal version of the given number.
    *
    * For example, given 1, returns "1st".
    */
  def ordinal(n: Int): String = {
    val suffixes = List(
      // irregular endings
      "11" -> "th",
      "12" -> "th",
      "13" -> "th",

      // regular-but-special endings
      "1" -> "st",
      "2" -> "nd",
      "3" -> "rd"
    )

    val defaultSuffix = "th"

    suffixes.collectFirst {
      case (end, suffix) if n.toString.endsWith(end) => n.toString + suffix
    }.getOrElse(n.toString + defaultSuffix)
  }
}
