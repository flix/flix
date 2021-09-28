/*
 * Copyright 2021 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for code hints.
  */
trait CodeHint extends CompilationError {
  def kind: String = "Code Hint"

  override def severity: Severity = Severity.Hint
}

object CodeHint {

  // TODO: DOC
  case class UsePureFunction(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Use of impure function prevents laziness / fusion."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine // TODO
      vt << ">> TODO" << NewLine
      vt << NewLine
      vt << Code(loc, "TODO") << NewLine // TODO
    }
  }


}