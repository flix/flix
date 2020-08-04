/*
 * Copyright 2016 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{Kind, Scheme, SourceLocation, Type}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType, TypeDiff}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt._

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationError {
  def kind: String = "Type Error"
}

object TypeError {
  implicit val audience: Audience = Audience.External

  /**
    * Generalization Error.
    *
    * @param declared the declared type scheme.
    * @param inferred the inferred type scheme.
    * @param loc      the location where the error occurred.
    */
  case class GeneralizationError(declared: Scheme, inferred: Scheme, loc: SourceLocation) extends TypeError {
    def summary: String = s"The type scheme '${FormatScheme.formatScheme(inferred)}' cannot be generalized to '${FormatScheme.formatScheme(declared)}'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> The type scheme: '" << Red(FormatScheme.formatScheme(inferred)) << "' cannot be generalized to '" << Red(FormatScheme.formatScheme(declared)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unable to generalize the type scheme.") << NewLine
      vt << "Possible fixes:" << NewLine
      vt << NewLine
      vt << "  (1) The function is declared as too polymorphic. Remove some type variables." << NewLine
      vt << "  (2) The expression body of the function is incorrect." << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * Mismatched Types.
    *
    * @param baseType1 the first base type.
    * @param baseType2 the second base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedTypes(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the types '$fullType1' and '$fullType2'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the types: '" << Red(FormatType.formatType(baseType1)) << "' and '" << Red(FormatType.formatType(baseType2)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched types.") << NewLine
      vt << NewLine
      vt << "Type One: " << FormatType.formatTypeDiff(TypeDiff.diff(fullType1, fullType2), Cyan) << NewLine
      vt << "Type Two: " << FormatType.formatTypeDiff(TypeDiff.diff(fullType2, fullType1), Magenta) << NewLine
    }
  }

  /**
    * Mismatched Boolean Formulas.
    *
    * @param baseType1 the first boolean formula.
    * @param baseType2 the second boolean formula.
    * @param fullType1 the first optional full type in which the first boolean formula occurs.
    * @param fullType2 the second optional full type in which the second boolean formula occurs.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedBools(baseType1: Type, baseType2: Type, fullType1: Option[Type], fullType2: Option[Type], loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the Boolean formulas '$baseType1' and '$baseType2'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the Boolean formulas: '" << Red(FormatType.formatType(baseType1)) << "' and '" << Red(FormatType.formatType(baseType2)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched boolean formulas.") << NewLine
      (fullType1, fullType2) match {
        case (Some(ft1), Some(ft2)) =>
          vt << "Type One: " << Cyan(FormatType.formatType(ft1)) << NewLine
          vt << "Type Two: " << Magenta(FormatType.formatType(ft2)) << NewLine
          vt << NewLine
        case _ => // nop
      }
      vt << "If the Boolean formula describes purity:" << NewLine
      vt << NewLine
      vt << "  (1) Did you forget to mark the function as impure?" << NewLine
      vt << "  (2) Are you trying to pass a pure function where an impure is required?" << NewLine
      vt << "  (3) Are you trying to pass an impure function where a pure is required?" << NewLine
      vt << NewLine
      vt << "If the Boolean formula describes nullability:" << NewLine
      vt << NewLine
      vt << "  (1) Are you trying to pass null where a non-null value is required?" << NewLine
      vt << NewLine
      vt
    }
  }

  /**
    * Mismatched kinds.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param kind1 the first kind.
    * @param kind2 the second kind.
    * @param loc the location where the error occurred.
    */
  case class MismatchedKinds(tpe1: Type, tpe2: Type, kind1: Kind, kind2: Kind, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the kinds '$kind1' and '$kind2'.'"

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the types: '" << Red(FormatType.formatType(tpe1)) << "' and '" << Red(FormatType.formatType(tpe2)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched kinds.") << NewLine
      vt << NewLine
      vt << "Kind One: " << Cyan(kind1.toString) << NewLine
      vt << "Kind Two: " << Magenta(kind2.toString) << NewLine
    }
  }

  /**
    * Mismatched Arity.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param loc  the location where the error occurred.
    */
  case class MismatchedArity(tpe1: Type, tpe2: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the types '$tpe1' and '$tpe2'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the types: '" << Red(FormatType.formatType(tpe1)) << "' and '" << Red(FormatType.formatType(tpe2)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched arity of types.") << NewLine
    }
  }

  /**
    * Occurs Check.
    *
    * @param baseVar   the base type variable.
    * @param baseType  the base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param loc       the location where the error occurred.
    */
  case class OccursCheckError(baseVar: Type.Var, baseType: Type, fullType1: Type, fullType2: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the type variable '$baseVar' with the type '$baseType'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the type variable '" << Red(baseVar.toString) << "' with the type '" << Red(FormatType.formatType(baseType)) << "'." << NewLine
      vt << ">> The type variable occurs recursively within the type." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched types.") << NewLine
      vt << NewLine
      vt << "Type One: " << FormatType.formatTypeDiff(TypeDiff.diff(fullType1, fullType2), Cyan) << NewLine
      vt << "Type Two: " << FormatType.formatTypeDiff(TypeDiff.diff(fullType2, fullType1), Magenta) << NewLine
    }
  }

  /**
    * Undefined field error.
    *
    * @param fieldName  the name of the missing field.
    * @param fieldType  the type of the missing field.
    * @param recordType the record type where the field is missing.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedField(fieldName: String, fieldType: Type, recordType: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Missing field '$fieldName' of type '$fieldType'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Missing field '" << Red(fieldName) << "' of type '" << Cyan(FormatType.formatType(fieldType)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "missing field.") << NewLine
      vt << "The record type: " << Indent << NewLine
      vt << NewLine
      vt << FormatType.formatType(recordType) << NewLine
      vt << Dedent << NewLine
      vt << "does not contain the field '" << Red(fieldName) << "' of type " << Cyan(FormatType.formatType(fieldType)) << "." << NewLine
    }
  }

  /**
    * Undefined predicate error.
    *
    * @param predName   the missing predicate.
    * @param predType   the type of the missing predicate.
    * @param schemaType the schema type where the predicate is missing.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedPredicate(predName: String, predType: Type, schemaType: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Missing predicate '$predName' of type '$predType'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Missing predicate '" << Red(predName) << "' of type '" << Cyan(FormatType.formatType(predType)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "missing predicate.") << NewLine
      vt << "The schema type: " << Indent << NewLine
      vt << NewLine
      vt << FormatType.formatType(schemaType) << NewLine
      vt << Dedent << NewLine
      vt << "does not contain the predicate '" << Red(predName) << "' of type " << Cyan(FormatType.formatType(predType)) << "." << NewLine
    }
  }

  /**
    * Unexpected non-record type error.
    *
    * @param tpe the unexpected non-record type.
    * @param loc the location where the error occurred.
    */
  case class NonRecordType(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unexpected non-record type '$tpe'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unexpected non-record type: '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unexpected non-record type.") << NewLine
    }
  }

  /**
    * Unexpected non-schema type error.
    *
    * @param tpe the unexpected non-schema type.
    * @param loc the location where the error occurred.
    */
  case class NonSchemaType(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unexpected non-schema type '$tpe'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unexpected non-schema type: '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "unexpected non-schema type.") << NewLine
    }
  }

  /**
    * Cast to an uninhabited type.
    *
    * @param tpe the uninhabited type.
    * @param loc the location where the error occurred.
    */
  case class UninhabitedTypeCast(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Uninhabited type cast."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Cast to uninhabited type: '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "cast to uninhabited type.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Effect casts can be written with the syntax 'f as & eff'."
    }
  }

  /**
    * Effect cast to non-bool type.
    *
    * @param tpe the non-bool type.
    * @param loc the location where the error occurred.
    */
  case class NonBoolEffectCast(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Non-bool effect cast."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Effect cast to non-bool type: '" << Red(FormatType.formatType(tpe)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "effect cast to non-bool type.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Regular casts can be written with the syntax 'x as t'."
    }
  }

  case class KindError(loc: SourceLocation) extends TypeError { // MATT fill in
    def summary: String = ""

    def message: VirtualTerminal = new VirtualTerminal
  }
}