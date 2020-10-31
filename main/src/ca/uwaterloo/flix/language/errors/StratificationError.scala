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

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a constraint set is not stratified.
  */
case class StratificationError(cycle: List[(Name.Ident, SourceLocation)], tpe: Type, loc: SourceLocation) extends CompilationError {
  private implicit val audience: Audience = Audience.External
  def kind: String = "Stratification Error"
  def summary: String = "The expression is not stratified. A predicate depends negatively on itself."
  def message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> The expression is not stratified. A predicate depends negatively on itself." << NewLine
    vt << NewLine
    vt << Code(loc, "the expression is not stratified.")
    vt << NewLine
    vt << "The type of the expression is:"
    vt << Indent << NewLine << NewLine
    vt << Cyan(FormatType.formatType(tpe))
    vt << Dedent << NewLine << NewLine
    vt << "The following predicate symbols are on the negative cycle:" << NewLine
    vt << Indent << NewLine
    vt << cycle.map(_._1).mkString(" <- ")
    vt << Dedent << NewLine
    vt << NewLine
    vt << "The following constraints are part of the negative cycle:" << NewLine
    vt << Indent
    for ((sym, loc) <- cycle) {
      vt << NewLine << Cyan(sym.name) << " at " << loc.format << " (which depends on)"
    }
    vt << Dedent << NewLine
  }

}
