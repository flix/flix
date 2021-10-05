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
}

object CodeHint {

  /**
    * A code hint that indicates that an operation could be lazy if given a pure function.
    *
    * @param sym the symbol of the operation that could be lazy.
    * @param loc the location associated with the code hint.
    */
  case class LazyWhenPure(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Use of impure function prevents lazy evaluation."

    override def severity: Severity = Severity.Hint

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Use of impure function prevents lazy evaluation." << NewLine
      vt << NewLine
      vt << Code(loc, "use of impure function.") << NewLine
    }
  }

  /**
    * A code hint that indicates that an operation could be parallel if given a pure function.
    *
    * @param sym the symbol of the operation that could be parallel.
    * @param loc the location associated with the code hint.
    */
  case class ParallelWhenPure(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Use of impure function prevents parallel evaluation."

    override def severity: Severity = Severity.Hint

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Use of impure function prevents parallel evaluation." << NewLine
      vt << NewLine
      vt << Code(loc, "use of impure function.") << NewLine
    }
  }

  /**
    * A code hint that indicates an expression has a non-trivial effect.
    *
    * @param loc the location of the expression.
    */
  case class NonTrivialEffect(loc: SourceLocation) extends CodeHint {
    override def summary: String = s"Expression has a non-trivial effect."

    override def severity: Severity = Severity.Info

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Expression has a non-trivial effect." << NewLine
      vt << NewLine
      vt << Code(loc, "non-trivial effect.") << NewLine
    }
  }

}
