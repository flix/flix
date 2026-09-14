/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * Represents the text of a variable.
  */
sealed trait VarText {

  /**
    * A measure of precision of the text.
    */
  private def precision: Int = this match {
    case VarText.Absent => 0
    case VarText.SourceText(_) => 2
  }

  /**
    * Returns true if `this` VarText is less precise than `that` VarText.
    *
    * More precise text should be preferred when choosing a text to use when substituting.
    *
    */
  def isStrictlyLessPreciseThan(that: VarText): Boolean = this.precision < that.precision
}

object VarText {
  /**
    * The variable has no associated text.
    */
  case object Absent extends VarText

  /**
    * The variable is associated with the string `s` taken directly from the source code.
    */
  case class SourceText(s: String) extends VarText
}
