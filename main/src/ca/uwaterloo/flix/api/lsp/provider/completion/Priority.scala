/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

/**
  * Represents the priority of a completion.
  *
  * A priority consists of an (A) absolute priority (e.g., `High` or `Low`) and (B) a relative priority (e.g., `High(5)`).
  */
sealed trait Priority extends Ordered[Priority] {
  /**
    * Returns the absolute priority of `this`.
    */
  def absolute: Int = this match {
    case Priority.Highest(_) => 0
    case Priority.Higher(_) => 1
    case Priority.High(_) => 2
    case Priority.Medium(_) => 3
    case Priority.Low(_) => 4
    case Priority.Lower(_) => 5
    case Priority.Lowest(_) => 6
  }

  /**
    * Returns the relative priority of `this`.
    *
    * A relative priority may be negative.
    */
  def relative: Int

  /** Compares `this` to `that` */
  override def compare(that: Priority): Int = {
    (this, that) match {
      case (Priority.Highest(r1), Priority.Highest(r2)) => r2 - r1
      case (Priority.Higher(r1), Priority.Higher(r2)) => r2 - r1
      case (Priority.High(r1), Priority.High(r2)) => r2 - r1
      case (Priority.Medium(r1), Priority.Medium(r2)) => r2 - r1
      case (Priority.Low(r1), Priority.Low(r2)) => r2 - r1
      case (Priority.Lower(r1), Priority.Lower(r2)) => r2 - r1
      case (Priority.Lowest(r1), Priority.Lowest(r2)) => r2 - r1
      case _ => that.absolute - this.absolute
    }
  }
}

object Priority {
  case class Highest(relative: Int) extends Priority

  case class Higher(relative: Int) extends Priority

  case class High(relative: Int) extends Priority

  case class Medium(relative: Int) extends Priority

  case class Low(relative: Int) extends Priority

  case class Lower(relative: Int) extends Priority

  case class Lowest(relative: Int) extends Priority

  /**
    * Returns the given string `l` prefixed with the absolute and relative priority.
    *
    * The idea is to return a string whose lexicographic ordering matches the
    * implicit ordering of the absolute and relative priority plus the label.
    */
  def toSortText(p: Priority, l: String): String = {
    val a = p.absolute
    val r = if (p.relative < 0) "0-" else "1-" + p.relative.abs.toString.padTo(4, '0')
    s"$a-$r-$l"
  }

}
