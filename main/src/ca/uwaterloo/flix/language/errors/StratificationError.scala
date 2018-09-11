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
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head.{False, True}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate that a constraint set is not stratified.
  */
case class StratificationError(constraints: List[TypedAst.Constraint], loc: SourceLocation) extends CompilationError {
  val kind: String = "Stratification Error"
  val source: Source = loc.source
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Stratification Error in Constraints" << NewLine

    vt << "The following constraint" << (if (constraints.size > 1) {
      "s"
    } else {
      ""
    }) << " form a negative cycle at locations" << NewLine
    constraints.foreach(rule => {
      vt << "\t" << (rule.head match {
        case True(loc) => loc.format
        case False(loc) => loc.format
        case Head.RelAtom(base, sym, terms, loc) => loc.format
        case Head.LatAtom(base, sym, terms, loc) => loc.format
      })
      vt << NewLine
    })
    vt
  }
}
