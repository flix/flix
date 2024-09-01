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
    * Returns a string representing `n` things called `name`, with the correct plural form,
    * assuming that `name` is a regular noun that can be pluralized by appending an "s".
    *
    * For example:
    *  - `n_things(1, "cat")` -> "1 cat"
    *  - `n_things(2, "cat")` -> "2 cats"
    */
  def n_things(n: Int, name: String): String = {
    val plural = n != 1
    if (plural)
      s"$n ${name}s"
    else
      s"$n $name"
  }

  /**
    * Returns a string representing that `n` things 'are', with the correct plural form.
    *
    * For example:
    *  - `n_are(1)` -> "1 is"
    *  - `n_are(2)` -> "2 are"
    */
  def n_are(n: Int): String = {
    val plural = n != 1
    if (plural)
      s"$n are"
    else
      s"$n is"
  }
}
