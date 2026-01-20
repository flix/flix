/*
 *  Copyright 2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, TraitUsageKind}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, UnkindedType}
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.util.{Formatter, Grammar}

/**
  * A common super-type for resolution errors.
  */
sealed trait ResolutionError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.ResolutionError
}

object ResolutionError {

  /**
    * An error raised to indicate a cycle in the trait hierarchy.
    *
    * @param path the super trait path from a trait to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTraitHierarchy(path: List[Symbol.TraitSym], loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9067

    def summary: String = s"Cyclic trait hierarchy: ${path.map(_.name).mkString(", ")}."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cyclic trait hierarchy: ${red(path.map(_.name).mkString(", "))}.
         |
         |${src(loc, "cyclic inheritance")}
         |
         |${underline("Explanation:")} A trait cannot directly or indirectly extend itself.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a cycle in type aliases.
    *
    * @param path the type reference path from a type alias to itself.
    * @param loc  the location where the error occurred.
    */
  case class CyclicTypeAliases(path: List[Symbol.TypeAliasSym], loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9178

    def summary: String = s"Cyclic type aliases: ${path.map(_.name).mkString(", ")}."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Cyclic type aliases: ${red(path.map(_.name).mkString(", "))}.
         |
         |${src(loc, "cyclic type aliases")}
         |
         |${underline("Explanation:")} A type alias cannot directly or indirectly reference itself.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a duplicate associated type definition.
    *
    * @param sym  the associated type symbol.
    * @param loc1 the location of the first associated type definition.
    * @param loc2 the location of the second associated type definition.
    */
  case class DuplicateAssocTypeDef(sym: Symbol.AssocTypeSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9281

    def summary: String = s"Duplicate associated type definition: $sym."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate associated type definition: ${red(sym.name)}.
         |
         |${src(loc1, "first occurrence")}
         |
         |${src(loc2, "duplicate")}
         |""".stripMargin
    }

    val loc: SourceLocation = loc1
  }

  /**
    * An error raised to indicate a duplicate derivation.
    *
    * @param sym  the trait symbol of the duplicate derivation.
    * @param loc1 the location of the first occurrence.
    * @param loc2 the location of the second occurrence.
    */
  case class DuplicateDerivation(sym: Symbol.TraitSym, loc1: SourceLocation, loc2: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9394

    def summary: String = s"Duplicate derivation: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Duplicate derivation '${red(sym.name)}'.
         |
         |${src(loc1, "first occurrence")}
         |
         |${src(loc2, "duplicate")}
         |""".stripMargin
    }

    def loc: SourceLocation = loc1

  }

  /**
    * An error raised to indicate a `new` struct expression provides an extra unknown field.
    *
    * @param sym   the symbol of the struct.
    * @param field the name of the extra field.
    * @param loc   the location where the error occurred.
    */
  case class ExtraStructFieldInNew(sym: Symbol.StructSym, field: Name.Label, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9407

    def summary: String = s"Unexpected field '$field' in new struct expression"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected field '${red(field.toString)}' in new struct expression.
         |
         |>> The struct '${cyan(sym.toString)}' does not declare a '${red(field.toString)}' field.
         |
         |${src(loc, "unexpected field")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that an associated type application is not allowed.
    *
    * @param loc the location where the error occurred.
    */
  case class IllegalAssocTypeApplication(loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9512

    def summary: String = "Unexpected associated type application."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected associated type application.
         |
         |${src(loc, "unexpected application")}
         |
         |${underline("Explanation:")} An associated type may only be applied to a type variable.
         |
         |  Elm[a]     // allowed
         |  Elm[Int32] // not allowed
         |""".stripMargin
    }
  }

  /**
    * Illegal Non-Java Type Error.
    *
    * @param tpe the illegal type.
    * @param loc the location where the error occurred.
    */
  case class IllegalNonJavaType(tpe: UnkindedType, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9623

    def summary: String = "Unexpected non-Java type. Expected class or interface type."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected non-Java type: '${red(tpe.toString)}'.
         |
         |${src(loc, "unexpected type")}
         |
         |${underline("Explanation:")} Expected a Java class or interface.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that a signature does not include the trait's type parameter.
    *
    * @param sym the symbol of the signature.
    * @param loc the location where the error occurred.
    */
  case class IllegalSignature(sym: Symbol.SigSym, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9736

    def summary: String = s"Unexpected signature which does not mention the type variable of the trait."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unexpected signature '${red(sym.name)}' which does not mention the type variable of the trait.
         |
         |${src(loc, "unexpected signature.")}
         |
         |${underline("Explanation:")}
         |Every signature in a trait must mention the type variable of the trait.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a wildcard type is used in an illegal position.
    *
    * @param ident the name of the wildcard type.
    * @param loc   the location where the error occurred.
    */
  case class IllegalWildType(ident: Name.Ident, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9843

    def summary: String = s"Illegal wildcard type: '$ident'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Illegal wildcard type: '$ident'.
         |
         |${src(loc, "illegal wildcard type.")}
         |
         |${underline("Explanation:")}
         |Wildcard types (types starting with an underscore) are not allowed in this position.
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a `put` struct expression attempts to modify an immutable field.
    *
    * @param field the immutable field.
    * @param loc   the location where the error occurred.
    */
  case class ImmutableField(field: Symbol.StructFieldSym, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E9956

    def summary: String = s"Modification of immutable field `${field.name}`."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Modification of immutable field '${red(field.name)}' on ${cyan(field.structSym.toString)}'.
         |
         |${src(loc, "immutable field")}
         |
         |Mark the field as 'mut' in the declaration of the struct.
         |""".stripMargin
    }
  }

  /**
    * Inaccessible Trait Error.
    *
    * @param sym the trait symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTrait(sym: Symbol.TraitSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0124

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Trait '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible trait.")}
         |
         |${underline("Tip:")} Mark the trait as public.
         |""".stripMargin

    }
  }

  /**
    * Inaccessible Def Error.
    *
    * @param sym the def symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleDef(sym: Symbol.DefnSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0237

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible definition.")}
         |
         |${underline("Tip:")} Mark the definition as public.
         |""".stripMargin
    }

  }

  /**
    * Inaccessible Effect Error.
    *
    * @param sym the effect symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEffect(sym: Symbol.EffSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0348

    def summary: String = s"Inaccessible alias ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Effect '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible effect.")}
         |
         |${underline("Tip:")} Mark the effect as public.
         |""".stripMargin
    }

  }

  /**
    * Inaccessible Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleEnum(sym: Symbol.EnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0459

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible enum.")}
         |
         |${underline("Tip:")} Mark the definition as public.
         |""".stripMargin
    }

  }

  /**
    * Inaccessible Struct Error
    *
    * @param sym the struct symbol
    * @param ns  the namespace where the symbol is not accessible
    * @param loc the location where the error occurred
    */
  case class InaccessibleStruct(sym: Symbol.StructSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0562

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Struct '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible struct.")}
         |
         |${underline("Tip:")} Mark the definition as public.
         |""".stripMargin
    }
  }

  /**
    * Inaccessible Restrictable Enum Error.
    *
    * @param sym the enum symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleRestrictableEnum(sym: Symbol.RestrictableEnumSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0673

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Enum '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible enum.")}
         |
         |${underline("Tip:")} Mark the definition as public.
         |""".stripMargin
    }

  }

  /**
    * Inaccessible Sig Error.
    *
    * @param sym the sig symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleSig(sym: Symbol.SigSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0784

    def summary: String = "Inaccessible."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Definition '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible definition.")}
         |
         |${underline("Tip:")} Mark the definition as public.
         |""".stripMargin
    }

  }

  /**
    * Inaccessible Type Alias Error.
    *
    * @param sym the type alias symbol.
    * @param ns  the namespace where the symbol is not accessible.
    * @param loc the location where the error occurred.
    */
  case class InaccessibleTypeAlias(sym: Symbol.TypeAliasSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0895

    def summary: String = s"Inaccessible type alias ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Type alias '${red(sym.toString)}' is not accessible from the namespace '${cyan(ns.toString)}'.
         |
         |${src(loc, "inaccessible type alias.")}
         |
         |${underline("Tip:")} Mark the type alias as public.
         |""".stripMargin
    }

  }

  /**
    * An error indicating the number of effect operation parameters does not match the expected number.
    *
    * @param op       the effect operation symbol.
    * @param expected the expected number of parameters.
    * @param actual   the actual number of parameters.
    * @param loc      the location where the error occurred.
    */
  case class MismatchedOpArity(op: Symbol.OpSym, expected: Int, actual: Int, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E0912

    def summary: String = s"Expected ${Grammar.n_things(expected, "parameter")} but found $actual."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched arity.
         |
         |The operation $op expects ${Grammar.n_things(expected, "parameter")},
         |but ${Grammar.n_are(actual)} provided here.
         |
         |${src(loc, s"expected ${Grammar.n_things(expected, "parameter")} but found $actual")}
         |""".stripMargin
    }
  }

  /**
    * An error indicating that the number of arguments to a tag-pattern is mismatched.
    *
    * @param caze     the enum case symbol.
    * @param expected the expected number of terms.
    * @param actual   the actual number of patterns.
    * @param loc      the location where the error occurred.
    */
  case class MismatchedTagPatternArity(caze: Symbol.CaseSym, expected: Int, actual: Int, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1023

    def summary: String = s"Expected ${Grammar.n_things(expected, "argument")} but found ${Grammar.n_things(actual, "argument")}."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""" Expected ${Grammar.n_things(expected, "argument")} for ${cyan(caze.toString)} but found ${Grammar.n_things(actual, "argument")}.
         |
         |${src(loc, s"mismatched arguments")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a missing associated type definition.
    *
    * @param name the name of the missing associated type definition.
    * @param loc  the location of the instance symbol where the error occurred.
    */
  case class MissingAssocTypeDef(name: String, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1134

    def summary: String = s"Missing associated type definition: $name."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing associated type definition: $name.
         |
         |${src(loc, s"missing associated type definition: $name.")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a handler is missing a definition.
    *
    * @param sym the symbol of the missing definition.
    * @param loc the location where the error occurred.
    */
  case class MissingHandlerDef(sym: Symbol.OpSym, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1245

    def summary: String = s"Missing handler definition: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing handler definition '${red(sym.name)}' for effect ${cyan(sym.eff.name)}'.
         |
         |${src(loc, "missing handler definition")}
         |
         |Add a handler definition for ${sym.name}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a `new` struct expression is missing a field.
    *
    * @param sym   the symbol of the struct.
    * @param field the name of the missing fields.
    * @param loc   the location where the error occurred.
    */
  case class MissingStructFieldInNew(sym: Symbol.StructSym, field: Name.Label, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1356

    def summary: String = s"Missing struct field '$field' in new struct expression"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing struct field '${red(field.toString)}' in new struct expression for struct '${cyan(sym.toString)}'.
         |
         |${src(loc, "missing field")}
         |""".stripMargin
    }
  }

  /**
    * Sealed Trait Error.
    *
    * @param sym the trait symbol.
    * @param ns  the namespace from which the trait is sealed.
    * @param loc the location where the error occurred.
    */
  case class SealedTrait(sym: Symbol.TraitSym, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1467

    def summary: String = "Sealed trait."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Trait '${red(sym.toString)}' is sealed from the module '${cyan(ns.toString)}'.
         |
         |${src(loc, "sealed trait.")}
         |
         |${underline("Tip:")} Move the instance or subtrait to the trait's module.
         |""".stripMargin
    }

  }

  /**
    * Undefined associated type error.
    *
    * @param qn  associated type.
    * @param loc the location where the error occurred.
    */
  case class UndefinedAssocType(qn: Name.QName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1578

    def summary: String = s"Undefined associated type: '$qn'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined associated type'${red(qn.toString)}'.
         |
         |${src(loc, "associated type not found.")}
         |
         |${underline("Tip:")} Possible typo or non-existent associated type?
         |""".stripMargin
    }
  }

  /**
    * Undefined Effect Error.
    *
    * @param qn  the unresolved effect.
    * @param ap  then anchor position.
    * @param ns  the current namespace.
    * @param scp the local scope.
    * @param loc the location where the error occurred.
    */
  case class UndefinedEffect(qn: Name.QName, ap: AnchorPosition, scp: LocalScope, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1689

    def summary: String = s"Undefined effect '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined effect '${red(qn.toString)}'.
         |
         |${src(loc, "effect not found")}
         |
         |${underline("Tip:")} Possible typo or non-existent effect?
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that a class name was not found.
    *
    * @param name the class name.
    * @param ap   the anchor position.
    * @param msg  the Java error message.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmClass(name: Name.Ident, ap: AnchorPosition, msg: String, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1792

    def summary: String = s"Undefined Java class: '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined Java class '${red(name.name)}'.
         |
         |${src(loc, "undefined class.")}
         |
         |$msg
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate that the class name in an importing was not found.
    *
    * @param name the class name.
    * @param ap   the anchor position.
    * @param msg  the Java error message.
    * @param loc  the location of the class name.
    */
  case class UndefinedJvmImport(name: String, ap: AnchorPosition, msg: String, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1803

    def summary: String = s"Undefined class in Java Import: '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined class in Java Import '${red(name)}'.
         |
         |${src(loc, "undefined class.")}
         |
         |$msg
         |$nestedClassHint
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    private def nestedClassHint: String = {
      if (raw".*\.[A-Z].*\.[A-Z].*".r matches name)
        s"Static nested classes should be specified using '$$', e.g. java.util.Locale$$Builder"
      else
        ""
    }
  }

  /**
    * An error raised to indicate that a static field name was not found.
    *
    * @param clazz the class name.
    * @param field the field name.
    * @param loc   the location of the field access.
    */
  case class UndefinedJvmStaticField(clazz: Class[?], field: Name.Ident, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E1914

    def summary: String = s"Undefined static field."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined static field '${red(field.name)}' in class '${cyan(clazz.getName)}'.
         |
         |${src(loc, "undefined static field.")}
         |""".stripMargin
    }
  }

  /**
    * Undefined Kind Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedKind(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2025

    def summary: String = s"Undefined kind: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined kind '${red(qn.toString)}'.
         |
         |${src(loc, "undefined kind.")}
         |
         |${underline("Tip:")} Possible typo or non-existent kind?
         |""".stripMargin
    }
  }

  /**
    * Undefined Name Error.
    *
    * @param qn  the unresolved name.
    * @param ap  the anchor position.
    * @param scp the variables in the scope.
    * @param loc the location where the error occurred.
    */
  case class UndefinedName(qn: Name.QName, ap: AnchorPosition, scp: LocalScope, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2136

    def summary: String = s"Undefined name: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined name '${red(qn.toString)}'.
         |
         |${src(loc, "name not found")}
         |
         |${underline("Tip:")} Possible typo or non-existent definition?
         |""".stripMargin
    }

  }

  /**
    * Undefined Name Error (unrecoverable).
    *
    * @param qn  the unresolved name.
    * @param ns  the current namespace.
    * @param scp the variables in the scope.
    * @param loc the location where the error occurred.
    */
  case class UndefinedNameUnrecoverable(qn: Name.QName, ns: Name.NName, scp: LocalScope, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2247

    def summary: String = s"Undefined name: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined name '${red(qn.toString)}'.
         |
         |${src(loc, "name not found")}
         |
         |${underline("Tip:")} Possible typo or non-existent definition?
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that class/struct name was not found in a new expression.
    *
    * @param name the jvm class/struct name
    * @param ap   the anchor position.
    * @param scp  the local scope.
    * @param msg  the error message.
    * @param loc  the location of the class/struct name.
    */
  case class UndefinedNewJvmClassOrStruct(name: Name.Ident, ap: AnchorPosition, scp: LocalScope, msg: String, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2358

    def summary: String = s"Undefined New: '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined New '${red(name.name)}'.
         |
         |${src(loc, "undefined new.")}
         |
         |$msg
         |""".stripMargin
    }
  }

  /**
    * Undefined Op Error.
    *
    * @param qn  the qualified name of the operation.
    * @param loc the location where the error occurred.
    */
  case class UndefinedOp(qn: Name.QName, ap: AnchorPosition, scp: LocalScope, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2469

    def summary: String = s"Undefined operation '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined operation '${red(qn.toString)}'.
         |
         |${src(loc, "operation not found")}
         |
         |${underline("Tip:")} Possible typo or non-existent operation?
         |""".stripMargin
    }
  }

  /**
    * Undefined Restrictable Tag Error.
    *
    * @param tag the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRestrictableTag(tag: String, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2572

    def summary: String = s"Undefined restrictable tag: '$tag'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined restrictable tag '${red(tag)}'.
         |
         |${src(loc, "tag not found.")}
         |
         |${underline("Tip:")} Possible typo or non-existent tag?
         |""".stripMargin
    }

  }

  /**
    * Undefined Restrictable Type Error.
    *
    * @param qn  the name.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedRestrictableType(qn: Name.QName, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2683

    def summary: String = s"Undefined restrictable type: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined restrictable type '${red(qn.toString)}'.
         |
         |${src(loc, "type not found.")}
         |
         |${underline("Tip:")} Possible typo or non-existent type?
         |""".stripMargin
    }

  }

  /**
    * Undefined Tag Error.
    *
    * @param qn  the tag.
    * @param ns  the current namespace.
    * @param loc the location where the error occurred.
    */
  case class UndefinedTag(qn: Name.QName, ap: AnchorPosition, scp: LocalScope, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2794

    def summary: String = s"Undefined tag: '$qn'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined tag '${red(qn.toString)}'.
         |
         |${src(loc, "tag not found.")}
         |
         |${underline("Tip:")} Possible typo or non-existent tag?
         |""".stripMargin
    }

  }

  /**
    * Undefined Trait Error.
    *
    * @param qn           the unresolved trait.
    * @param traitUseKind the kind of trait use.
    * @param ap           the anchor position.
    * @param scp          the variables in the scope.
    * @param ns           the current namespace.
    * @param loc          the location where the error occurred.
    */
  case class UndefinedTrait(qn: Name.QName, traitUseKind: TraitUsageKind, ap: AnchorPosition, scp: LocalScope, ns: Name.NName, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2805

    def summary: String = s"Undefined trait: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined trait '${red(qn.toString)}'.
         |
         |${src(loc, "trait not found")}
         |
         |${underline("Tip:")} Possible typo or non-existent trait?
         |""".stripMargin
    }

  }

  /**
    * Undefined Type Error.
    *
    * @param qn      the name.
    * @param kindOpt the kind of the type.
    * @param ap      the enclosing module.
    * @param loc     the location where the error occurred.
    */
  case class UndefinedType(qn: Name.QName, kindOpt: Option[Kind], ap: AnchorPosition, scp: LocalScope, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E2916

    def summary: String = s"Undefined type: '${qn.toString}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined type '${red(qn.toString)}'.
         |
         |${src(loc, "type not found.")}
         |
         |${underline("Tip:")} Possible typo or non-existent type?
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3027

    def summary: String = s"Undefined type variable '$name'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined type variable '${red(name)}'.
         |
         |${src(loc, "undefined type variable.")}
         |
         |${underline("Explanation:")}
         |Flix cannot find the type variable. Maybe there is a typo?
         |""".stripMargin

    }
  }

  /**
    * Undefined Use Error (unrecoverable).
    *
    * @param qn  the unresolved name.
    * @param ns  the current namespace.
    * @param env the variables in the scope.
    * @param loc the location where the error occurred.
    */
  case class UndefinedUse(qn: Name.QName, ns: Name.NName, env: Map[String, Symbol.VarSym], loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3138

    def summary: String = s"Undefined '${qn.toString}' use."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined '${red(qn.toString)}' use.
         |
         |${src(loc, "name not found")}
         |
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the associated type.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedAssocType(sym: Symbol.AssocTypeSym, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3249

    def summary: String = s"Under-applied associated type: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Under-applied associated type '${red(sym.name)}'.
         |
         |${src(loc, "Under-applied associated type.")}
         |
         |${underline("Tip:")} Associated types must be fully applied.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate an under-applied type alias.
    *
    * @param sym the type alias.
    * @param loc the location where the error occurred.
    */
  case class UnderAppliedTypeAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3352

    def summary: String = s"Under-applied type alias: ${sym.name}"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Under-applied type alias '${red(sym.name)}'.
         |
         |${src(loc, "Under-applied type alias.")}
         |
         |${underline("Tip:")} Type aliases must be fully applied.
         |""".stripMargin
    }

  }

  /**
    * An error raised to indicate an undefined struct in a `new S { ... } @ r` expression.
    *
    * @param name the name of the undefined struct.
    * @param loc  the location where the error occurred.
    */
  case class UndefinedStruct(name: Name.QName, ap: AnchorPosition, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3463

    def summary: String = s"Undefined struct"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined struct '${red(name.toString)}'.
         |
         |${src(loc, "undefined struct")}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate an undefined struct field in a struct get or struct put expression.
    *
    * @param struct the optional symbol of the struct.
    * @param field  the name of the missing field.
    * @param loc    the location where the error occurred.
    */
  case class UndefinedStructField(struct: Option[Symbol.StructSym], field: Name.Label, loc: SourceLocation) extends ResolutionError {
    def code: ErrorCode = ErrorCode.E3574

    def summary: String = s"Undefined struct field '$field'$structMessage"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Undefined struct field '${red(field.toString)}'$structMessage.
         |
         |${src(loc, "undefined field")}
         |""".stripMargin
    }

    private def structMessage: String = struct match {
      case Some(sym) => s" on struct '$sym'."
      case None => ""
    }
  }

}
