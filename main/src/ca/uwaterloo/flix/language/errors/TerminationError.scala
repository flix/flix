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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for termination errors.
  */
sealed trait TerminationError extends CompilationError {
  override def kind: String = "Termination Error"
}

object TerminationError {
  case class UnconditionalDefRecursion(defn: Symbol.DefnSym, loc: SourceLocation) extends TerminationError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }

  case class UnconditionalSigRecursion(sig: Symbol.SigSym, loc: SourceLocation) extends TerminationError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }
}
