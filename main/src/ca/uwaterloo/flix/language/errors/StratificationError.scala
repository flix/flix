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
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.{Filter, Loop, Negative, Positive}
import ca.uwaterloo.flix.language.ast.TypedAst.Stratum
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a property is violated.
  */
case class StratificationError(stratum: List[Stratum], head: Symbol.TableSym, body: TypedAst.Predicate.Body, bodySym: Symbol.TableSym) extends CompilationError {
  val kind: String = "Stratification Error"
  val source: Source = constraints.head.loc.source
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Stratification Error in constraints" << NewLine
    vt << "It was triggered by the rule: " << head.name << "(...) :- "
    vt << (body match {
      case Positive(sym, terms, loc) => sym.name + "(...)"
      case Negative(sym, terms, loc) => "!" + sym.name + "(...)"
      case Filter(name, terms, loc) => name.name + "(...)"
      case Loop(sym, term, loc) => ???
    })
    vt << NewLine << "At " << body.loc.format
    vt << NewLine
  }
}
