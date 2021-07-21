/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.debug.FormatTypeConstraint
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Green, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for termination errors.
  */
sealed trait TerminationError extends CompilationError {
  override def kind: String = "Termination Error"
}

object TerminationError {

  /**
    * An error raised to indicate that the given definition recurses unconditionally.
    *
    * @param sym the unconditionally recursive definition.
    */
  case class UnconditionalRecursion(sym: Symbol.DefnSym) extends TerminationError {
    def summary: String = "Unconditional recursion."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unconditionally recursive definition '" << Red(sym.name) << "'. All branches will recurse indefinitely." << NewLine
      vt << NewLine
      vt << Code(sym.loc, "unconditional recursion.") << NewLine
      vt << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1)  Add a non-recursive branch to the definition." << NewLine
      vt << NewLine
      vt
    }

    def loc: SourceLocation = sym.loc
  }
}
