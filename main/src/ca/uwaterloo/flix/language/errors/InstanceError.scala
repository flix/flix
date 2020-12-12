/*
 * Copyright 2020 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme}
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine, Underline}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for instance errors.
  */
sealed trait InstanceError extends CompilationError {
  def kind: String = "Instance Error"
}

object InstanceError {
  private implicit val audience: Audience = Audience.External

  /**
    * Error indicating that the types of two instances overlap.
    *
    * @param loc1 the location of the first instance.
    * @param loc2 the location of the second instance.
    */
  case class OverlappingInstances(loc1: SourceLocation, loc2: SourceLocation) extends InstanceError {
    def summary: String = "Overlapping instances."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << Code(loc1, "the first instance was declared here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second instance was declared here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or change the type of one of the instances." << NewLine
    }

    def loc: SourceLocation = loc1 min loc2
  }

  /**
    * Error indicating that the type scheme of a definition does not match the type scheme of the signature it implements.
    *
    * @param loc      the location of the definition
    * @param expected the scheme of the signature
    * @param actual   the scheme of the definition
    */
  case class MismatchedSignatures(loc: SourceLocation, expected: Scheme, actual: Scheme) extends InstanceError {
    def summary: String = "Mismatched signature."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched signature.") << NewLine
      vt << NewLine
      vt << s"Expected scheme: ${FormatScheme.formatScheme(expected)}" << NewLine
      vt << s"Actual scheme: ${FormatScheme.formatScheme(actual)}" << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Modify the definition to match the signature."
    }
  }

  /**
    * Error indicating the instance is missing a signature implementation.
    *
    * @param sig the missing signature.
    * @param loc the location of the instance.
    */
  case class MissingImplementation(sig: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    def summary: String = "Missing implementation."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << Code(loc, s"The signature ${sig.name} is missing from the instance.")
      vt << NewLine
      vt << Underline("Tip:") << " Add an implementation of the signature to the instance."
    }
  }

  /**
    * Error indicating the instance has a definition not present in the implemented class.
    *
    * @param defn the extraneous definition.
    * @param loc  the location of the definition.
    */
  case class ExtraneousDefinition(defn: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    def summary: String = "Extraneous implementation."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << Code(loc, s"The signature ${defn.name} is not present in the class.")
      vt << NewLine
      vt << Underline("Tip:") << " Remove this definition from the instance."
    }
  }

  // MATT docs
  case class DuplicateTypeParameter(tvar: Type.Var, loc: SourceLocation) extends InstanceError {
    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }

  // MATT docs
  case class ComplexInstanceType(tpe: Type, loc: SourceLocation) extends InstanceError {

    override def summary: String = "" // MATT

    override def message: VirtualTerminal = new VirtualTerminal() // MATT
  }

}
