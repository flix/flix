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
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a constraint set is not stratified.
  */
case class StratificationError(cycle: List[(Symbol.PredSym, SourceLocation)], loc: SourceLocation) extends CompilationError {
  val kind: String = "Stratification Error"
  val source: Source = loc.source
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Stratification Error." << NewLine
    vt << NewLine
    vt << Code(loc, "the constraint set is not stratified.")
    vt << NewLine
    vt << "The following predicate symbols form a negative cycle:" << NewLine
    vt << Indent << NewLine
    vt << cycle.map(_._1).mkString(", ")
    vt << Dedent << NewLine
    vt << NewLine
    vt << "The constraints at the following locations are on the cycle:" << NewLine
    vt << Indent
    for ((sym, loc) <- cycle) {
      vt << NewLine << Cyan(sym.toString) << " at " << loc.format << " (which depends on)"
    }
    vt << Dedent << NewLine
  }
}
