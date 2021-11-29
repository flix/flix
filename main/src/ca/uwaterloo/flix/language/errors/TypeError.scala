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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.debug.{Audience, FormatEff, FormatScheme, FormatType}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationMessage {
  val kind: String = "Type Error"
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
    def summary: String = s"The type scheme '${FormatScheme.formatSchemeWithoutConstraints(inferred)}' cannot be generalized to '${FormatScheme.formatSchemeWithoutConstraints(declared)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> The type scheme: '${red(FormatScheme.formatSchemeWithoutConstraints(inferred))}' cannot be generalized to '${red(FormatScheme.formatSchemeWithoutConstraints(declared))}'.
         |
         |${code(loc, "unable to generalize the type scheme.")}
         |
         |The declared type does not match the inferred type:
         |
         |  Declared: ${cyan(FormatScheme.formatSchemeWithoutConstraints(declared))}
         |  Inferred: ${magenta(FormatScheme.formatSchemeWithoutConstraints(inferred))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      """The declared type is more polymorphic than the inferred type. In other words,
        |the declared type has more type variables than the inferred type. Either:
        |
        |  (a) the declared type signature is incorrect, or
        |  (b) the implementation is incorrect.
        |""".stripMargin)
  }

  /**
    * Effect Generalization Error.
    *
    * @param declared the declared effect.
    * @param inferred the inferred effect.
    * @param loc      the location where the error occurred.
    */
  case class EffectGeneralizationError(declared: Type, inferred: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"The inferred effect '${FormatEff.formatEff(inferred)}' cannot be generalized to '${FormatEff.formatEff(declared)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> The inferred effect: '${red(FormatEff.formatEff(inferred))}' cannot be generalized to '${red(FormatEff.formatEff(declared))}'.
         |
         |${code(loc, "unable to generalize the effect.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unable to unify the types: '${red(FormatType.formatType(baseType1))}' and '${red(FormatType.formatType(baseType2))}'.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${FormatType.formatType(fullType1)}
         |Type Two: ${FormatType.formatType(fullType2)}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unable to unify the Boolean formulas: '${red(FormatType.formatType(baseType1))}' and '${red(FormatType.formatType(baseType2))}'.
         |
         |${code(loc, "mismatched boolean formulas.")}
         |
         |${appendMismatchedBooleans(formatter)}
         |""".stripMargin
    }

    private def appendMismatchedBooleans(formatter: Formatter): String = (fullType1, fullType2) match {
      case (Some(ft1), Some(ft2)) =>
        import formatter._
        s"""Type One: ${cyan(FormatType.formatType(ft1))}
           |Type Two: ${magenta(FormatType.formatType(ft2))}
           |""".stripMargin
      case _ => "" // nop
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""If the Boolean formula describes purity:
         |
         |  (1) Did you forget to mark the function as impure?
         |  (2) Are you trying to pass a pure function where an impure is required?
         |  (3) Are you trying to pass an impure function where a pure is required?
         |
         |If the Boolean formula describes nullability:
         |
         |  (1) Are you trying to pass null where a non-null value is required?
         |
         |""".stripMargin
    })
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unable to unify the types: '${red(FormatType.formatType(tpe1))}' and '${red(FormatType.formatType(tpe2))}'.
         |
         |${code(loc, "mismatched arity of types.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
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
  case class OccursCheckError(baseVar: Type.KindedVar, baseType: Type, fullType1: Type, fullType2: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unable to unify the type variable '$baseVar' with the type '$baseType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unable to unify the type variable '${red(baseVar.toString)}' with the type '${red(FormatType.formatType(baseType))}'.
         |
         |>> The type variable occurs recursively within the type.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${FormatType.formatType(fullType1)}
         |Type Two: ${FormatType.formatType(fullType2)}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Missing field '${red(field.name)}' of type '${cyan(FormatType.formatType(fieldType))}'.
         |
         |${code(loc, "missing field.")}
         |
         |The record type:
         |
         |  ${FormatType.formatType(recordType)}
         |
         |does not contain the field '${red(field.name)}' of type ${cyan(FormatType.formatType(fieldType))}.
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
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

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Missing predicate '${red(pred.name)}' of type '${cyan(FormatType.formatType(predType))}'.
         |
         |${code(loc, "missing predicate.")}
         |
         |The schema type:
         |
         |  ${FormatType.formatType(schemaType)}
         |
         |does not contain the predicate '${red(pred.name)}' of type ${cyan(FormatType.formatType(predType))}.
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected non-record type error.
    *
    * @param tpe the unexpected non-record type.
    * @param loc the location where the error occurred.
    */
  case class NonRecordType(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unexpected non-record type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unexpected non-record type: '${red(FormatType.formatType(tpe))}'.
         |
         |${code(loc, "unexpected non-record type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected non-schema type error.
    *
    * @param tpe the unexpected non-schema type.
    * @param loc the location where the error occurred.
    */
  case class NonSchemaType(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Unexpected non-schema type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Unexpected non-schema type: '${red(FormatType.formatType(tpe))}'.
         |
         |${code(loc, "unexpected non-schema type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * No matching instance error.
    *
    * @param clazz the class of the instance.
    * @param tpe   the type of the instance.
    * @param loc   the location where the error occurred.
    */
  case class NoMatchingInstance(clazz: Symbol.ClassSym, tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"No instance of class '$clazz' for type '${FormatType.formatType(tpe)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> No instance of class '${red(clazz.toString)}' for type ${red(FormatType.formatType(tpe))}.
         |
         |${code(loc, s"missing instance")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """A type class operation is called on a type that does not have that operation.
        |You may want to implement an instance for that type.
        |""".stripMargin
    })
  }

  /**
    * Missing `Eq` instance.
    *
    * @param tpe the type of the instance.
    * @param loc the location where the error occurred.
    */
  case class MissingEq(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Equality is not defined on '${FormatType.formatType(tpe)}'. Define or derive instance of Eq."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Equality is not defined on ${red(FormatType.formatType(tpe))}. Define or derive an instance of Eq.
         |
         |${code(loc, s"missing Eq instance")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To define equality on '${FormatType.formatType(tpe)}', either:
         |
         |  (a) define an instance of Eq for '${FormatType.formatType(tpe)}', or
         |  (b) derive an instance of Eq for '${FormatType.formatType(tpe)}'.
         |
         |To automatically derive an instance, you can write:
         |
         |  enum Color with Eq {
         |    case Red, Green, Blue
         |  }
         |
         |The following types does not support any notion of equality:
         |
         |    - function types.
         |    - records and schemas.
         |    - mutable data structures (e.g. references, arrays).
         |""".stripMargin
    })
  }

  /**
    * Missing `Order` instance.
    *
    * @param tpe the type of the instance.
    * @param loc the location where the error occurred.
    */
  case class MissingOrder(tpe: Type, loc: SourceLocation) extends TypeError {
    def summary: String = s"Order is not defined on '${FormatType.formatType(tpe)}'. Define or derive instance of Order."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> Order is not defined on ${red(FormatType.formatType(tpe))}. Define or derive an instance of Order.
         |
         |${code(loc, s"missing Order instance")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To define an order on '${FormatType.formatType(tpe)}', either:
         |
         |  (a) define an instance of Order for '${FormatType.formatType(tpe)}', or
         |  (b) derive an instance of Order for '${FormatType.formatType(tpe)}'.
         |
         |To automatically derive an instance, you can write:
         |
         |  enum Color with Eq, Order {
         |    case Red, Green, Blue
         |  }
         |
         |The following types does not support any notion of order:
         |
         |    - function types.
         |    - records and schemas.
         |    - mutable data structures (e.g. references, arrays).
         |""".stripMargin
    })
  }

  /**
    * An error indicating that the main function's scheme is incorrect.
    *
    * @param declaredScheme the erroneous function's scheme.
    * @param expectedScheme the scheme the main function is expected to have.
    * @param loc            the location where the error occurred.
    */
  case class IllegalMain(declaredScheme: Scheme, expectedScheme: Scheme, loc: SourceLocation) extends TypeError {
    override def summary: String = "Illegal main."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.format)}
         |>> The main function has an unexpected type.
         |
         |${code(loc, s"unexpected type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""The main function must have the form:
         |
         |  def main(args: Array[String]): Int32 & Impure = ...
         |
         |i.e.
         |- it must return an integer which is the exit code, and
         |- it must have a side-effect (such as printing to the screen).
         |
         |If the arguments `args` are not needed they can be replaced by an '_'.
         |""".stripMargin
    })
  }
}
