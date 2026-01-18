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
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.{FormatEqualityConstraint, FormatScheme, FormatTraitConstraint}
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for instance errors.
  */
sealed trait InstanceError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.InstanceError
}

object InstanceError {

  /**
    * Error indicating a complex instance type.
    *
    * @param tpe the complex type.
    * @param sym the trait symbol.
    * @param loc the location where the error occurred.
    */
  case class ComplexInstance(tpe: Type, sym: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E1952

    def summary: String = s"Complex type '${formatType(tpe)}' in instance declaration for trait '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Complex type '${red(formatType(tpe))}' in instance declaration for trait '${magenta(sym.name)}'.
         |
         |${src(loc, "expected a type constructor applied to distinct type variables")}
         |
         |${underline("Explanation:")} An instance type must be either:
         |  - A concrete type (e.g., Bool, Int32, String), or
         |  - A type constructor applied to zero or more distinct type variables.
         |
         |${underline("Examples")} of ${green("simple")} (allowed) instance types:
         |  - Bool, Int32, String
         |  - Option[a], List[a], Map[k, v]
         |  - (a, b), (a, b, c)
         |
         |${underline("Examples")} of ${red("complex")} (not allowed) instance types:
         |  - Option[Int32]       (type argument is not a variable)
         |  - List[Option[a]]     (type argument is not a variable)
         |  - (a, a)              (type variable 'a' appears twice)
         |  - Map[k, k]           (type variable 'k' appears twice)
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
  case class DuplicateTypeVar(tvar: Type.Var, sym: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2063

    def summary: String = s"Duplicate type variable '${formatType(tvar)}' in instance declaration for trait '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate type variable '${red(formatType(tvar))}' in instance declaration for trait '${magenta(sym.name)}'.
         |
         |${src(loc, "duplicate occurrence")}
         |
         |${underline("Explanation:")} Each type variable in an instance type must be distinct.
         |Rename one of the occurrences to make them unique.
         |
         |${underline("Example:")}
         |
         |  instance C[(a, a)]        // Not allowed: 'a' appears twice
         |  instance C[(a, b)]        // OK: 'a' and 'b' are distinct
         |""".stripMargin
    }

  }

  /**
    * Error indicating the instance has a definition not present in the implemented trait.
    *
    * @param defnSym  the defn symbol.
    * @param traitSym the trait symbol.
    * @param loc      the location of the definition.
    */
  case class ExtraneousDef(defnSym: Symbol.DefnSym, traitSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2176

    def summary: String = s"Extraneous definition '${defnSym.name}' in instance declaration for trait '${traitSym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Extraneous definition '${red(defnSym.name)}' in instance declaration for trait '${magenta(traitSym.name)}'.
         |
         |${src(loc, "not present in trait")}
         |
         |${underline("Explanation:")} An instance can only implement signatures declared in the trait.
         |The definition '${defnSym.name}' does not exist in trait '${traitSym.name}'.
         |""".stripMargin
    }
  }

  /**
    * Error indicating an associated type in an instance type.
    *
    * @param assoc  the type alias.
    * @param trtSym the trait symbol.
    * @param loc    the location where the error occurred.
    */
  case class IllegalAssocTypeInstance(assoc: Symbol.AssocTypeSym, trtSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2289

    def summary: String = s"Unexpected associated type '${assoc.name}' in instance declaration for trait '${trtSym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected associated type '${red(assoc.name)}' in instance declaration for trait '${magenta(trtSym.name)}'.
         |
         |${src(loc, "associated type not allowed here")}
         |
         |${underline("Explanation:")} Instances cannot be defined on associated types.
         |""".stripMargin
    }
  }

  /**
    * Error indicating the illegal placement of an override modifier.
    *
    * @param sym the def that the modifier was applied to.
    * @param loc the location where the error occurred.
    */
  case class IllegalOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2394

    def summary: String = s"Unexpected redefinition of '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected redefinition of '${red(sym.name)}'.
         |
         |${src(loc, "no default implementation to redefine")}
         |
         |${underline("Explanation:")} The signature '${sym.name}' is defined in the trait,
         |but there is no default implementation to be redefined.
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
  case class IllegalTypeAliasInstance(alias: Symbol.TypeAliasSym, trtSym: Symbol.TraitSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2401

    def summary: String = s"Unexpected type alias '${alias.name}' in instance declaration for trait '${trtSym.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected type alias '${red(alias.name)}' in instance declaration for trait '${magenta(trtSym.name)}'.
         |
         |${src(loc, "type alias not allowed here")}
         |
         |${underline("Explanation:")} Instances cannot be defined on type aliases.
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
  case class MismatchedSignatures(sigSym: Symbol.SigSym, loc: SourceLocation, expected: Scheme, actual: Scheme)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2518

    def summary: String = s"Mismatched signature '${sigSym.name}' in instance declaration for trait '${sigSym.trt.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched signature '${red(sigSym.name)}' in instance declaration for trait '${magenta(sigSym.trt.name)}'.
         |
         |${src(loc, "signature mismatch")}
         |
         |${cyan("Expected")}: ${FormatScheme.formatScheme(expected)}
         |${red("Actual")}:   ${FormatScheme.formatScheme(actual)}
         |
         |${underline("Explanation:")} The implementation must match the signature declared in the trait.
         |""".stripMargin
    }
  }

  /**
    * An error indicating that a required equality constraint is missing from an instance declaration.
    *
    * @param econstr    the missing constraint.
    * @param superTrait the supertrait that is the source of the constraint.
    * @param loc        the location where the error occurred.
    */
  case class MissingEqualityConstraint(econstr: EqualityConstraint, superTrait: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2629

    def summary: String = s"Missing equality constraint '${FormatEqualityConstraint.formatEqualityConstraint(econstr)}' required by super trait '${superTrait.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing equality constraint '${red(FormatEqualityConstraint.formatEqualityConstraint(econstr))}' required by super trait '${magenta(superTrait.name)}'.
         |
         |${src(loc, "missing constraint")}
         |
         |${underline("Explanation:")} The super trait '${superTrait.name}' requires this equality constraint to be satisfied.
         |Add the constraint to the instance declaration.
         |""".stripMargin
    }
  }

  /**
    * Error indicating the instance is missing a signature implementation.
    *
    * @param sig the missing signature.
    * @param loc the location of the instance.
    */
  case class MissingImplementation(sig: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2736

    def summary: String = s"Missing implementation of '${sig.name}' in instance declaration for trait '${sig.trt.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing implementation of '${red(sig.name)}' in instance declaration for trait '${magenta(sig.trt.name)}'.
         |
         |${src(loc, "required by trait")}
         |
         |${underline("Explanation:")} The trait declares the signature '${sig.name}', but this instance
         |does not provide an implementation for it.
         |
         |${underline("Hint:")} Did you misspell the signature name?
         |
         |${underline("Example:")}
         |
         |  trait T[a] {
         |      pub def f(): a
         |  }
         |
         |  instance T[Int32] { }                            // Missing 'f'
         |  instance T[Int32] { pub def f(): Int32 = 123 }   // OK
         |""".stripMargin
    }
  }

  /**
    * Error indicating a missing super trait instance.
    *
    * @param tpe        the type for which the super trait instance is missing.
    * @param subTrait   the symbol of the sub trait.
    * @param superTrait the symbol of the super trait.
    * @param loc        the location where the error occurred.
    */
  case class MissingSuperTraitInstance(tpe: Type, subTrait: Symbol.TraitSym, superTrait: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2843

    def summary: String = s"Missing instance of super trait '${superTrait.name}' for type '${formatType(tpe)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing instance of super trait '${red(superTrait.name)}' for type '${magenta(formatType(tpe))}'.
         |
         |${src(loc, "missing super trait instance")}
         |
         |${underline("Explanation:")} The trait '${subTrait.name}' extends '${superTrait.name}'.
         |An instance of '${subTrait.name}' requires a corresponding instance of '${superTrait.name}'.
         |
         |${underline("Example:")}
         |
         |  trait A[t]
         |  trait B[t] with A[t]
         |
         |  instance B[Bool]               // Missing instance of A[Bool]
         |  instance A[Int32]
         |  instance B[Int32]              // OK: super trait instance provided
         |""".stripMargin
    }
  }

  /**
    * An error indicating that a required constraint is missing from an instance declaration.
    *
    * @param tconstr    the missing constraint.
    * @param superTrait the supertrait that is the source of the constraint.
    * @param loc        the location where the error occurred.
    */
  case class MissingTraitConstraint(tconstr: TraitConstraint, superTrait: Symbol.TraitSym, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E2956

    def summary: String = s"Missing trait constraint '${FormatTraitConstraint.formatTraitConstraint(tconstr)}' required by super trait '${superTrait.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing trait constraint '${red(FormatTraitConstraint.formatTraitConstraint(tconstr))}' required by super trait '${magenta(superTrait.name)}'.
         |
         |${src(loc, "missing constraint")}
         |
         |${underline("Explanation:")} The super trait '${superTrait.name}' requires this trait constraint to be satisfied.
         |Add the constraint to the instance declaration.
         |
         |${underline("Example:")}
         |
         |  trait C[a]
         |  trait D[a] with C[a]
         |
         |  instance D[(a, b)]                       // Missing C[a], C[b]
         |  instance C[Map[a, b]]
         |  instance D[Map[a, b]] with C[a], C[b]    // OK
         |""".stripMargin
    }
  }

  /**
    * Error indicating an orphan instance.
    *
    * @param sym the trait symbol.
    * @param tpe the instance type.
    * @param loc the location where the error occurred.
    */
  case class OrphanInstance(sym: Symbol.TraitSym, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends InstanceError {
    def code: ErrorCode = ErrorCode.E3067

    def summary: String = s"Orphan instance of '${sym.name}' for type '${formatType(tpe)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Orphan instance of '${magenta(sym.name)}' for type '${red(formatType(tpe))}'.
         |
         |${src(loc, "orphan instance")}
         |
         |${underline("Explanation:")} An instance must be declared in the trait's namespace or in the type's namespace.
         |
         |${underline("Example:")} Correct instance declarations:
         |
         |  mod A {
         |      pub trait T[a]
         |      instance T[Int32]          // OK: in the trait's namespace
         |  }
         |
         |  mod B {
         |      use A.T
         |      pub enum Box[a] { case Box(a) }
         |      instance T[Box[a]]         // OK: in the type's namespace
         |  }
         |""".stripMargin
    }
  }

  /**
    * Error indicating that the types of two instances overlap.
    *
    * @param sym  the trait symbol.
    * @param tc   the type constructor that overlaps.
    * @param loc1 the location of the first instance.
    * @param loc2 the location of the second instance.
    */
  case class OverlappingInstances(sym: Symbol.TraitSym, tc: TypeConstructor, loc1: SourceLocation, loc2: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E3178

    def summary: String = s"Overlapping instances of '${sym.name}' for type '$tc'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Overlapping instances of '${magenta(sym.name)}' for type '${red(tc.toString)}'.
         |
         |${src(loc1, "first instance")}
         |
         |${src(loc2, "second instance")}
         |
         |${underline("Explanation:")} Two instances overlap if their types unify.
         |Each type can have at most one instance of a trait.
         |
         |${underline("Example:")} Overlapping instances:
         |
         |  trait T[a]
         |  instance T[List[a]]
         |  instance T[List[b]]    // Overlaps: List[a] and List[b] unify
         |""".stripMargin
    }

    def loc: SourceLocation = loc1
  }

  /**
    * Error indicating an unlawful signature in a lawful trait.
    *
    * @param sym the symbol of the unlawful signature.
    * @param loc the location where the error occurred.
    */
  case class UnlawfulSignature(sym: Symbol.SigSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E3281

    def summary: String = s"Unlawful signature '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unlawful signature '${red(sym.name)}'.
         |
         |>> Each signature of a lawful trait must appear in at least one law.
         |
         |${src(loc, s"unlawful signature")}
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
  case class UnmarkedOverride(sym: Symbol.DefnSym, loc: SourceLocation) extends InstanceError {
    def code: ErrorCode = ErrorCode.E3394

    def summary: String = s"Unmarked override '$sym'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unmarked override of '${red(sym.name)}'. This definition overrides a default implementation.
         |
         |${src(loc, s"unmarked override")}
         |
         |Either add the `override` modifier or remove the definition.
         |""".stripMargin
    }
  }
}
