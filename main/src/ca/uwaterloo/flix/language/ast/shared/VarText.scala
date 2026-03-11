/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
