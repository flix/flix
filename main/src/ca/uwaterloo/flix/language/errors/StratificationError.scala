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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.Format

/**
 * An error raised to indicate that a constraint set is not stratified.
 */
case class StratificationError(cycle: List[(Name.Pred, SourceLocation)], tpe: Type, loc: SourceLocation) extends CompilationMessage {
  private implicit val audience: Audience = Audience.External

  def kind: String = "Stratification Error"

  def summary: String = "The expression is not stratified. A predicate depends negatively on itself."

  def message: String = {
    s"""${Format.line(kind, source.format)}
       |>> The expression is not stratified. A predicate depends negatively on itself.
       |
       |${Format.code(loc, "the expression is not stratified.")}
       |The type of the expression is:
       |
       |  ${Format.cyan(FormatType.formatType(tpe))}
       |
       |The following predicate symbols are on the negative cycle:
       |
       |  ${cycle.map(_._1).mkString(" <- ")}
       |
       |The following constraints are part of the negative cycle:
       |$constraints
       |""".stripMargin
  }

  private def constraints: String = {
    cycle.map(t => "  " + Format.cyan(t._1.name) + " at " + t._2.format + " (which depends on)" + System.lineSeparator()).mkString
  }
}
