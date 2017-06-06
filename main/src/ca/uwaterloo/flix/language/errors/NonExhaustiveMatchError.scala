/*
 * Copyright 2017 Jason Mitterteiner
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate a non exhaustive pattern match
  */
case class NonExhaustiveMatchError(rules: List[TypedAst.MatchRule], pattern: String, loc: SourceLocation) extends CompilationError {
  val kind = "Exhaustive Match Error"
  val source: SourceInput = loc.source
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Exhaustive Match Error:" << NewLine
    vt << NewLine
    vt << "The expression: " << NewLine
    vt << Code(loc, "")
    vt << "Matched by the rules:" << NewLine
    rules.foreach(x => vt << Code(x.pat.loc, ""))
    vt << "is not exhaustive, consider the pattern: " << NewLine
    vt << Indent << Red(pattern)
    vt << NewLine
  }
}
