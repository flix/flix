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
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.util.vt.VirtualString._
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
    * @param sigSym   the mismatched signature
    * @param loc      the location of the definition
    * @param expected the scheme of the signature
    * @param actual   the scheme of the definition
    */
  case class MismatchedSignatures(sigSym: Symbol.SigSym, loc: SourceLocation, expected: Scheme, actual: Scheme) extends InstanceError {
    def summary: String = "Mismatched signature."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << NewLine
      vt << "Mismatched signature '" << Red(sigSym.name) << "' required by class '" << Red(sigSym.clazz.name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched signature.") << NewLine
      vt << NewLine
      vt << s"Expected scheme: ${FormatScheme.formatScheme(expected)}" << NewLine
      vt << s"Actual scheme:   ${FormatScheme.formatScheme(actual)}" << NewLine
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

  /**
    * Error indicating the duplicate use of a type variable in an instance type.
    *
    * @param tvar the duplicated type variable.
    * @param sym  the class symbol.
    * @param loc  the location where the error occurred.
    */
  case class DuplicateTypeVariableOccurrence(tvar: Type.Var, sym: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = "Duplicate type variable."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate type variable '" << Red(FormatType.formatType(tvar)) << "' in '" << Red(sym.name) << "'."
      vt << NewLine
      vt << Code(loc, s"The type variable '${FormatType.formatType(tvar)}' occurs more than once.")
      vt << NewLine
      vt << Underline("Tip:") << " Rename one of the instances of the type variable."
    }
  }

  /**
    * Error indicating a complex instance type.
    *
    * @param tpe the complex type.
    * @param sym the class symbol.
    * @param loc the location where the error occurred.
    */
  case class ComplexInstanceType(tpe: Type, sym: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = "Complex instance type."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Complex instance type '" << Red(FormatType.formatType(tpe)) << "' in '" << Red(sym.name) << "'."
      vt << NewLine
      vt << Code(loc, s"complex instance type")
      vt << NewLine
      vt << Underline("Tip:") << " An instance type must be a type constructor applied to zero or more distinct type variables."
    }
  }

  /**
    * Error indicating an orphan instance.
    *
    * @param tpe the instance type.
    * @param sym the class symbol.
    * @param loc the location where the error occurred.
    */
  case class OrphanInstance(tpe: Type, sym: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = "Orphan instance."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Orphan instance for type '" << Red(FormatType.formatType(tpe)) << "' in '" << Red(sym.name) << "'."
      vt << NewLine
      vt << Code(loc, s"orphan instance")
      vt << NewLine
      vt << Underline("Tip:") << " An instance must be declared in the class's namespace or in the type's namespace."
    }
  }

  /**
    * Error indicating a missing super class instance.
    *
    * @param tpe the type for which the super class instance is missing.
    * @param subClass the symbol of the sub class.
    * @param superClass the symbol of the super class.
    * @param loc the location where the error occurred.
    */
  case class MissingSuperClassInstance(tpe: Type, subClass: Symbol.ClassSym, superClass: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Missing super class instance '$superClass'."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Missing super class instance '" << Red(superClass.name) << "' for type '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << ">> The class '" << Red(subClass.name) << "' extends the class '" << Red(superClass.name) << "'." << NewLine
      vt << ">> If you provide an instance for '" << Red(subClass.name) << "' you must also provide an instance for '" << Red(superClass.name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, s"missing super class instance")
      vt << NewLine
      vt << Underline("Tip:") << s" Add an instance of '${superClass.name}' for '${FormatType.formatType(tpe)}'."
    }
  }

  /**
    * Error indicating an lawless super class of a lawful subclass.
    *
    * @param subClass   the lawful sub class.
    * @param superClass the lawless super class.
    * @param loc        the location where the error occurred.
    */
  case class LawlessSuperClass(subClass: Symbol.ClassSym, superClass: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Lawless super class '$superClass'."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Lawless super class '" << Red(superClass.name) << "'." << NewLine
      vt << NewLine
      vt << ">> The class '" << Red(subClass.name) << "' extends the lawless class '" << Red(superClass.name) << "'." << NewLine
      vt << ">> A lawful class cannot extend an unlawful class."
      vt << NewLine
      vt << Code(loc, s"lawless super class")
      vt << NewLine
      vt << Underline("Tip:") << s" Mark '${subClass.name}' as lawless."
    }
  }

  /**
    * Error indicating an unlawful signature in a lawful class.
    *
    * @param sym the symbol of the unlawful signature.
    * @param loc the location where the error occurred.
    */
  case class UnlawfulSignature(sym: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Unlawful signature '$sym'."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unlawful signature '" << Red(sym.name) << "'." << NewLine
      vt << NewLine
      vt << ">> Each signature of a lawful class must appear in at least one law."
      vt << NewLine
      vt << Code(loc, s"unlawful signature")
      vt << NewLine
      vt << Underline("Tip:") << s" Create a law for '${sym}' or mark the class as unlawful."
    }
  }
}
