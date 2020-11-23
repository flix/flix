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
import ca.uwaterloo.flix.language.ast._
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
//  implicit val audience: Audience = Audience.External // MATT
  implicit val audience: Audience = Audience.Internal

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
    * @param tpe1  the first type.
    * @param tpe2  the second type.
    * @param kind1 the first kind.
    * @param kind2 the second kind.
    * @param loc   the location where the error occurred.
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
    * @param field      the name of the missing field.
    * @param fieldType  the type of the missing field.
    * @param recordType the record type where the field is missing.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedField(field: Name.Field, fieldType: Type, recordType: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Missing field '$field' of type '$fieldType'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Missing field '" << Red(field.name) << "' of type '" << Cyan(FormatType.formatType(fieldType)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "missing field.") << NewLine
      vt << "The record type: " << Indent << NewLine
      vt << NewLine
      vt << FormatType.formatType(recordType) << NewLine
      vt << Dedent << NewLine
      vt << "does not contain the field '" << Red(field.name) << "' of type " << Cyan(FormatType.formatType(fieldType)) << "." << NewLine
    }
  }

  /**
    * Undefined predicate error.
    *
    * @param pred       the missing predicate.
    * @param predType   the type of the missing predicate.
    * @param schemaType the schema type where the predicate is missing.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedPredicate(pred: Name.Pred, predType: Type, schemaType: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Missing predicate '${pred.name}' of type '$predType'."

    def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Missing predicate '" << Red(pred.name) << "' of type '" << Cyan(FormatType.formatType(predType)) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "missing predicate.") << NewLine
      vt << "The schema type: " << Indent << NewLine
      vt << NewLine
      vt << FormatType.formatType(schemaType) << NewLine
      vt << Dedent << NewLine
      vt << "does not contain the predicate '" << Red(pred.name) << "' of type " << Cyan(FormatType.formatType(predType)) << "." << NewLine
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
    * Error indicating that the types of two instances overlap.
    *
    * @param loc1 the location of the first instance.
    * @param loc2 the location of the second instance.
    */
  case class OverlappingInstances(loc1: SourceLocation, loc2: SourceLocation) extends TypeError {
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
    * @param loc the location of the definition
    * @param expected the scheme of the signature
    * @param actual the scheme of the definition
    */
  case class MismatchedSignatures(loc: SourceLocation, expected: Scheme, actual: Scheme) extends TypeError {
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
    * @param sig the missing signature.
    * @param loc the location of the instance.
    */
  case class MissingImplementation(sig: Symbol.SigSym, loc: SourceLocation) extends TypeError {
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
    * @param defn the extraneous definition.
    * @param loc the location of the definition.
    */
  case class ExtraneousDefinition(defn: Symbol.DefnSym, loc: SourceLocation) extends TypeError {
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
}