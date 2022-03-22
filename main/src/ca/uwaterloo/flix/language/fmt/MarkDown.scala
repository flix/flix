package ca.uwaterloo.flix.language.fmt

object MarkDown {

  /**
    * Escapes certain markdown characters.
    */
  def escape(s: String): String =
    s
      .replace("[", "\\[")
      .replace("]", "\\]")

}
