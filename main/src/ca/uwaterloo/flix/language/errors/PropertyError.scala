/*
 *  Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{FinalAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a property is violated.
  */
case class PropertyError(property: FinalAst.Property, m: Map[Symbol.VarSym, String]) extends CompilationError {
  def kind: String = "Property Error"
  def summary: String = s"Function does not satisfy the law '${property.law}'."
  def message: VirtualTerminal = {
    val name = property.defn.toString
    val law = property.law.toString

    val vt = new VirtualTerminal()
    vt << Line(kind, source.format) << NewLine
    vt << ">> The function '" << Red(name) << "' does not satisfy the law '" << Cyan(law) << "'." << NewLine
    vt << NewLine
    vt << "Counter-example: " << m.map(p => p._1.text -> p._2).mkString(", ") << NewLine
    vt << NewLine
    vt << Code(property.defn.loc, s"violates the law '$law'.") << NewLine
    vt << NewLine
    vt << Underline("Details") << " The universal/existential quantifiers were instantiated as follows:" << NewLine
    vt << NewLine
    for ((sym, value) <- m) {
      vt << Code(sym.loc, s"instantiated as '$value'.") << NewLine
    }
    vt << NewLine
  }
  val loc: SourceLocation = property.defn.loc
}
