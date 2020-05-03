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
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate a non exhaustive pattern match expression.
  */
case class NonExhaustiveMatchError(rules: List[TypedAst.MatchRule], pat: String, loc: SourceLocation) extends CompilationError {
  def kind = "Pattern Match"
  def summary: String = s"Non-exhaustive match. Missing case: '$pat'."
  def message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Non-Exhaustive Pattern. Missing case: " << Red(pat) << " in match expression." << NewLine
    vt << NewLine
    vt << Code(loc, "incomplete pattern.")
    vt << NewLine
  }
}
