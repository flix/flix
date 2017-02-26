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
import ca.uwaterloo.flix.language.ast.{ExecutableAst, SourceInput, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString.{Cyan, Magenta, Red, Underline}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a property is violated.
  */
case class PropertyError(property: ExecutableAst.Property, m: Map[Symbol.VarSym, String]) extends CompilationError {
  val kind: String = "Property Error"
  val source: SourceInput = property.defn.loc.source
  val message: VirtualTerminal = {
    val result = new VirtualTerminal().
      header(kind, source).
      text(">> The function ").quote(Red(property.defn.toString)).text(" does not satisfy the law ").quote(Cyan(property.law.toString)).text(".").newLine().
      newLine().
      text("Counter-example: ").text(m.map(p => p._1.text -> p._2).mkString(", ")).newLine().
      newLine().
      highlight(property.defn.loc, "violates the law '", Cyan(property.law.toString), "'.").newLine().
      text(Underline("Details")).text(": The universal/existential quantifiers were instantiated as follows:").newLine().
      newLine()
    for ((sym, value) <- m) {
      result.highlight(sym.loc, "instantiated as '", Magenta(value), "'.").newLine()
    }
    result.newLine()
  }
}
