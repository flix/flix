/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * An error raised to indicate that a constraint set is not stratified.
  */
case class StratificationError(cycle: List[(Name.Pred, SourceLocation)], tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends CompilationMessage with Recoverable {
  def kind: String = "Stratification Error"

  def summary: String = "The expression is not stratified. A predicate depends strongly on itself."

  def message(formatter: Formatter): String = {
    import formatter._
    s""">> The expression is not stratified. A predicate depends strongly on itself.
       |
       |${code(loc, "the expression is not stratified.")}
       |
       |The type of the expression is:
       |
       |  ${cyan(FormatType.formatType(tpe))}
       |
       |The following predicate symbols are on the cycle:
       |
       |  ${cycle.map(_._1).mkString(" <- ")}
       |
       |The following constraints are part of the cycle:
       |${fmtConstraints(formatter)}
       |""".stripMargin
  }

  /**
    * Formats the constraint dependencies.
    */
  private def fmtConstraints(formatter: Formatter): String = {
    cycle.map(t => "  " + formatter.cyan(t._1.name) + " at " + t._2.format + " (which depends on)" + System.lineSeparator()).mkString
  }
}
