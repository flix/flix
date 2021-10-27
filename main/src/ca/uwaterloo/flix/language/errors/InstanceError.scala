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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for instance errors.
  */
sealed trait InstanceError extends CompilationMessage {
  val kind: String = "Instance Error"
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |${formatter.code(loc1, "the first instance was declared here.")}
         |
         |${formatter.code(loc2, "the second instance was declared here.")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip: ")} Remove or change the type of one of the instances.")
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

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |Mismatched signature '${formatter.red(sigSym.name)}' required by class '${formatter.red(sigSym.clazz.name)}'.
         |
         |${formatter.code(loc, "mismatched signature.")}
         |
         |Expected scheme: ${FormatScheme.formatScheme(expected)}
         |Actual scheme:   ${FormatScheme.formatScheme(actual)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Modify the definition to match the signature.")
  }

  /**
    * Error indicating the instance is missing a signature implementation.
    *
    * @param sig the missing signature.
    * @param loc the location of the instance.
    */
  case class MissingImplementation(sig: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    def summary: String = "Missing implementation."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |Missing implementation of '${formatter.red(sig.name)}' required by class '${formatter.red(sig.clazz.name)}'.
         |
         |${formatter.code(loc, s"missing implementation")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Add an implementation of the signature to the instance.")
  }

  /**
    * Error indicating the instance has a definition not present in the implemented class.
    *
    * @param defn the extraneous definition.
    * @param loc  the location of the definition.
    */
  case class ExtraneousDefinition(defn: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    def summary: String = "Extraneous implementation."

    def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |
         |${formatter.code(loc, s"The signature ${defn.name} is not present in the class.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Remove this definition from the instance.")
  }

  /**
    * Error indicating the duplicate use of a type variable in an instance type.
    *
    * @param tvar the duplicated type variable.
    * @param sym  the class symbol.
    * @param loc  the location where the error occurred.
    */
  case class DuplicateTypeVariableOccurrence(tvar: Type.KindedVar, sym: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = "Duplicate type variable."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Duplicate type variable '${formatter.red(FormatType.formatType(tvar))}' in '${formatter.red(sym.name)}'.
         |
         |${formatter.code(loc, s"The type variable '${FormatType.formatType(tvar)}' occurs more than once.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Rename one of the instances of the type variable.")

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

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Complex instance type '${formatter.red(FormatType.formatType(tpe))}' in '${formatter.red(sym.name)}'.
         |${formatter.code(loc, s"complex instance type")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} An instance type must be a type constructor applied to zero or more distinct type variables.")
  }

  /**
    * Error indicating a type alias in an instance type.
    *
    * @param alias the type alias.
    * @param clazz the class symbol.
    * @param loc   the location where the error occurred.
    */
  case class IllegalTypeAliasInstance(alias: Symbol.TypeAliasSym, clazz: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = "Type alias in instance type."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Illegal use of type alias '${formatter.red(alias.name)}' in instance declaration for '${formatter.red(clazz.name)}'.
         |${formatter.code(loc, s"illegal use of type alias")}
         |""".stripMargin

    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} A type class instance cannot use a type alias. Use the full type.")

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

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Orphan instance for type '${formatter.red(FormatType.formatType(tpe))}' in '${formatter.red(sym.name)}'.
         |${formatter.code(loc, s"orphan instance")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} An instance must be declared in the class's namespace or in the type's namespace.")
  }

  /**
    * Error indicating a missing super class instance.
    *
    * @param tpe        the type for which the super class instance is missing.
    * @param subClass   the symbol of the sub class.
    * @param superClass the symbol of the super class.
    * @param loc        the location where the error occurred.
    */
  case class MissingSuperClassInstance(tpe: Type, subClass: Symbol.ClassSym, superClass: Symbol.ClassSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Missing super class instance '$superClass'."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Missing super class instance '${formatter.red(superClass.name)}' for type '${formatter.red(FormatType.formatType(tpe))}'.
         |
         |>> The class '${formatter.red(subClass.name)}' extends the class '${formatter.red(superClass.name)}'.
         |>> If you provide an instance for '${formatter.red(subClass.name)}' you must also provide an instance for '${formatter.red(superClass.name)}'.
         |
         |${formatter.code(loc, s"missing super class instance")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Add an instance of '${superClass.name}' for '${FormatType.formatType(tpe)}'.")
  }

  /**
    * Error indicating an unlawful signature in a lawful class.
    *
    * @param sym the symbol of the unlawful signature.
    * @param loc the location where the error occurred.
    */
  case class UnlawfulSignature(sym: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Unlawful signature '$sym'."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Unlawful signature '${formatter.red(sym.name)}'.
         |
         |>> Each signature of a lawful class must appear in at least one law.
         |${formatter.code(loc, s"unlawful signature")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Create a law for '$sym' or mark the class as unlawful.")
  }

  /**
    * Error indicating the illegal placement of an override modifier.
    *
    * @param sym the def that the modifier was applied to.
    * @param loc the location where the error occurred.
    */
  case class IllegalOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Illegal override of '$sym'."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Illegal override of '${formatter.red(sym.name)}'.
         |
         |>> Only signatures with default implementations can be overridden.
         |
         |${formatter.code(loc, s"illegal override")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Remove the modifier.")
  }

  /**
    * Error indicating a missing override modifier.
    *
    * @param sym the def that is missing the modifier.
    * @param loc the location where the error occurred.
    */
  case class UnmarkedOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    override def summary: String = s"Unmarked override '$sym'."

    override def message(formatter: Formatter): String = {
      s"""${formatter.line(kind, source.format)}
         |>> Unmarked override of '${formatter.red(sym.name)}'. This definition overrides a default implementation.
         |
         |${formatter.code(loc, s"unmarked override")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(s"${formatter.underline("Tip:")} Either add the `override` modifier or remove the definition.")
  }

}
