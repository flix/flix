/*
 *  Copyright 2021 Magnus Madsen
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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for reification errors.
  */
sealed trait ReificationError extends CompilationMessage {
  def kind: String = "Monomorph Error"
}

object ReificationError {

  private implicit val audience: Audience = Audience.External

  /**
    * An error raised to indicate that the Boolean type cannot be reified.
    *
    * @param tpe the Boolean type that cannot be reified.
    * @param loc the location of the Boolean type.
    */
  case class IllegalReifiedBool(tpe: Type, loc: SourceLocation) extends ReificationError {
    def summary: String = "Type cannot be reified."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to reify the non-constant Bool '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unable to reify type.") << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate that the type cannot be reified.
    *
    * @param tpe the type that cannot be reified.
    * @param loc the location of the type.
    */
  case class IllegalReifiedType(tpe: Type, loc: SourceLocation) extends ReificationError {
    def summary: String = "Type cannot be reified."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to reify the type '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unable to reify type.") << NewLine
      vt
    }
  }

  /**
    * An error raised to indicate an internal error in the Monomorpher.
    *
    * @param tpe the problematic Boolean type.
    * @param loc the location of the Boolean type.
    */
  case class UnexpectedNonConstBool(tpe: Type, loc: SourceLocation) extends ReificationError {
    def summary: String = "Unexpected type."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unexpected Boolean type: '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unexpected Boolean type.") << NewLine
      vt
    }
  }

}
