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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.TraitConstraint
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatTraitConstraint, FormatType}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for instance errors.
  *
  * All [[InstanceError]]s are [[Recoverable]].
  */
sealed trait InstanceError extends CompilationMessage with Recoverable {
  val kind: String = "Instance Error"
}

object InstanceError {

  /**
    * Error indicating a complex instance type.
    *
    * @param tpe the complex type.
    * @param sym the trait symbol.
    * @param loc the location where the error occurred.
    */
  case class ComplexInstance(tpe: Type, sym: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError with Recoverable {
    override def summary: String = "Complex instance type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Complex instance type '${red(FormatType.formatType(tpe))}' in '${magenta(sym.name)}'.
         |
         |${code(loc, s"complex instance type")}
         |
         |An instance type must be a type constructor applied to zero or more distinct type variables.
         |""".stripMargin
    }
  }

  /**
    * Error indicating the duplicate use of a type variable in an instance type.
    *
    * @param tvar the duplicated type variable.
    * @param sym  the trait symbol.
    * @param loc  the location where the error occurred.
    */
  case class DuplicateTypeVar(tvar: Type.Var, sym: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError with Recoverable {
    override def summary: String = "Duplicate type variable."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate type variable '${red(FormatType.formatType(tvar))}' in '${magenta(sym.name)}'.
         |
         |${code(loc, s"The type variable '${FormatType.formatType(tvar)}' occurs more than once.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Rename one of the instances of the type variable."
    })

  }

  /**
    * Error indicating the instance has a definition not present in the implemented trait.
    *
    * @param defnSym  the defn symbol.
    * @param traitSym the trait symbol.
    * @param loc      the location of the definition.
    */
  case class ExtraneousDef(defnSym: Symbol.DefnSym, traitSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError with Recoverable {
    def summary: String = "Extraneous implementation."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> The signature '${red(defnSym.name)}' is not present in the '${magenta(traitSym.name)}' trait.
         |
         |${code(loc, s"extraneous def")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Remove this definition from the instance."
    })
  }

  /**
    * Error indicating an associated type in an instance type.
    *
    * @param assoc  the type alias.
    * @param trtSym the trait symbol.
    * @param loc    the location where the error occurred.
    */
  case class IllegalAssocTypeInstance(assoc: Symbol.AssocTypeSym, trtSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError with Recoverable {
    override def summary: String = "Associated type in instance type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal use of associated type '${red(assoc.name)}' in instance declaration for '${magenta(trtSym.name)}'.
         |
         |${code(loc, s"illegal use of associated type")}
         |
         |A trait instance cannot use an associated type. Use the full type.
         |""".stripMargin
    }
  }

  /**
    * Error indicating the illegal placement of an override modifier.
    *
    * @param sym the def that the modifier was applied to.
    * @param loc the location where the error occurred.
    */
  case class IllegalOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError with Recoverable {
    override def summary: String = s"Illegal override of '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal override of '${red(sym.name)}'.
         |
         |${code(loc, s"illegal override")}
         |
         |Only signatures with default implementations can be overridden.
         |
         |""".stripMargin
    }
  }

  /**
    * Error indicating a type alias in an instance type.
    *
    * @param alias  the type alias.
    * @param trtSym the trait symbol.
    * @param loc    the location where the error occurred.
    */
  case class IllegalTypeAliasInstance(alias: Symbol.TypeAliasSym, trtSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError with Recoverable {
    override def summary: String = "Type alias in instance type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal use of type alias '${red(alias.name)}' in instance declaration for '${magenta(trtSym.name)}'.
         |
         |${code(loc, s"illegal use of type alias")}
         |
         |A trait instance cannot use a type alias. Use the full type.
         |""".stripMargin
    }
  }

  /**
    * Error indicating that the type scheme of a definition does not match the type scheme of the signature it implements.
    *
    * @param sigSym   the mismatched signature
    * @param loc      the location of the definition
    * @param expected the scheme of the signature
    * @param actual   the scheme of the definition
    */
  case class MismatchedSignatures(sigSym: Symbol.SigSym, loc: SourceLocation, expected: Scheme, actual: Scheme)(implicit flix: Flix) extends InstanceError with Recoverable {
    def summary: String = "Mismatched signature."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched signature '${red(sigSym.name)}' required by '${magenta(sigSym.trt.name)}'.
         |
         |${code(loc, "mismatched signature.")}
         |
         |Expected scheme: ${FormatScheme.formatScheme(expected)}
         |Actual scheme:   ${FormatScheme.formatScheme(actual)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Modify the definition to match the signature."
    })
  }

  /**
    * Error indicating the instance is missing a signature implementation.
    *
    * @param sig the missing signature.
    * @param loc the location of the instance.
    */
  case class MissingImplementation(sig: Symbol.SigSym, loc: SourceLocation) extends InstanceError with Recoverable {
    def summary: String = "Missing implementation."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing implementation of '${red(sig.name)}' required by '${magenta(sig.trt.name)}'.
         |
         |${code(loc, s"missing implementation")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Add an implementation of the signature to the instance."
    })
  }

  /**
    * Error indicating a missing super trait instance.
    *
    * @param tpe        the type for which the super trait instance is missing.
    * @param Trait      the symbol of the sub trait.
    * @param superTrait the symbol of the super trait.
    * @param loc        the location where the error occurred.
    */
  case class MissingSuperTraitInstance(tpe: Type, subTrait: Symbol.TraitSym, superTrait: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError with Recoverable {
    override def summary: String = s"Missing super trait instance '$superTrait'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing super trait instance '${red(superTrait.name)}' for type '${red(FormatType.formatType(tpe))}'.
         |
         |${code(loc, s"missing super trait instance")}
         |
         |The trait '${red(subTrait.name)}' extends the trait '${red(superTrait.name)}'.
         |
         |If you provide an instance for '${red(subTrait.name)}' you must also provide an instance for '${red(superTrait.name)}'.
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Add an instance of '${superTrait.name}' for '${FormatType.formatType(tpe)}'."
    })
  }

  /**
    * An error indicating that a required constraint is missing from an instance declaration.
    *
    * @param tconstr    the missing constraint.
    * @param superTrait the supertrait that is the source of the constraint.
    * @param loc        the location where the error occurred.
    */
  case class MissingTraitConstraint(tconstr: TraitConstraint, superTrait: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError with Recoverable {
    override def summary: String = s"Missing type constraint: ${FormatTraitConstraint.formatTraitConstraint(tconstr)}"

    override def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing type constraint: ${FormatTraitConstraint.formatTraitConstraint(tconstr)}
         |
         |The constraint ${FormatTraitConstraint.formatTraitConstraint(tconstr)} is required because it is a constraint on super trait ${superTrait.name}.
         |
         |${code(loc, s"missing type constraint")}
      """.stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip:")} Add the missing type constraint."
    })
  }

  /**
    * Error indicating an orphan instance.
    *
    * @param sym the trait symbol.
    * @param tpe the instance type.
    * @param loc the location where the error occurred.
    */
  case class OrphanInstance(sym: Symbol.TraitSym, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends InstanceError with Recoverable {
    override def summary: String = "Orphan instance."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Orphan instance for type '${red(FormatType.formatType(tpe))}' in '${magenta(sym.name)}'.
         |
         |${code(loc, s"orphan instance")}
         |
         |An instance must be declared in the trait's namespace or in the type's namespace.
         |""".stripMargin
    }
  }

  /**
    * Error indicating that the types of two instances overlap.
    *
    * @param sym  the trait symbol.
    * @param loc1 the location of the first instance.
    * @param loc2 the location of the second instance.
    */
  case class OverlappingInstances(sym: Symbol.TraitSym, loc1: SourceLocation, loc2: SourceLocation) extends InstanceError with Recoverable {
    def summary: String = "Overlapping instances."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Overlapping instances for '${magenta(sym.name)}'.
         |
         |${code(loc1, "the first instance was declared here.")}
         |
         |${code(loc2, "the second instance was declared here.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter.*
      s"${underline("Tip: ")} Remove or change the type of one of the instances."
    })

    def loc: SourceLocation = loc1
  }

  /**
    * Error indicating an unlawful signature in a lawful trait.
    *
    * @param sym the symbol of the unlawful signature.
    * @param loc the location where the error occurred.
    */
  case class UnlawfulSignature(sym: Symbol.SigSym, loc: SourceLocation) extends InstanceError with Recoverable {
    override def summary: String = s"Unlawful signature '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unlawful signature '${red(sym.name)}'.
         |
         |>> Each signature of a lawful trait must appear in at least one law.
         |
         |${code(loc, s"unlawful signature")}
         |
         |Create a law for '$sym' or remove the 'lawful' modifier from the trait.
         |""".stripMargin
    }
  }

  /**
    * Error indicating a missing override modifier.
    *
    * @param sym the def that is missing the modifier.
    * @param loc the location where the error occurred.
    */
  case class UnmarkedOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError with Recoverable {
    override def summary: String = s"Unmarked override '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unmarked override of '${red(sym.name)}'. This definition overrides a default implementation.
         |
         |${code(loc, s"unmarked override")}
         |
         |Either add the `override` modifier or remove the definition.
         |""".stripMargin
    }
  }
}
