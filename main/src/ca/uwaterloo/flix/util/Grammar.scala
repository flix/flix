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

  /**
    * Returns a string representation of a count of `n` things called `name` that optionally `are` something.
    * When `n` is 1, the sentence will be singular, otherwise it will be plural.
    *
    * It is assumed that `name` is a regular noun, that can be pluralized by appending an "s".
    *
    * For example, given `count(1, Some("cat"), are=true)` returns "1 cat is",
    * and given `count(2, Some("cat"), are=true)` returns "2 cats are".
    *
    * The `name` argument is optional, for example `count(2, None, are=true)` returns "2 are".
    *
    * The `are` parameter can be false (default), for example `count(3, Some("cat"))` returns "3 cats".
    */
  def count(n: Int, name: Option[String], are: Boolean = false): String = {
    val plural = n != 1
    var s = n.toString

    name.foreach { ns =>
      s += " " + ns
      if (plural)
        s += "s"
    }

    if (are)
      if (plural)
        s += " are"
      else
        s += " is"

    s
  }
}
