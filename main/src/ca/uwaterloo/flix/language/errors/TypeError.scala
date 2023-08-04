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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.BroadEqualityConstraint
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.fmt.FormatEqualityConstraint.formatEqualityConstraint
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.language.fmt._
import ca.uwaterloo.flix.util.{Formatter, Grammar}

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationMessage {
  val kind: String = "Type Error"
}

object TypeError {

  /**
    * Generalization Error.
    *
    * @param declared the declared type scheme.
    * @param inferred the inferred type scheme.
    * @param loc      the location where the error occurred.
    */
  case class GeneralizationError(declared: Scheme, inferred: Scheme, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"The type scheme '${FormatScheme.formatSchemeWithOnlyEqualityConstraints(inferred)}' cannot be generalized to '${FormatScheme.formatSchemeWithOnlyEqualityConstraints(declared)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The type scheme: '${red(FormatScheme.formatSchemeWithOnlyEqualityConstraints(inferred))}' cannot be generalized to '${red(FormatScheme.formatSchemeWithOnlyEqualityConstraints(declared))}'.
         |
         |${code(loc, "unable to generalize the type scheme.")}
         |
         |The declared type does not match the inferred type:
         |
         |  Declared: ${cyan(FormatScheme.formatSchemeWithOnlyEqualityConstraints(declared))}
         |  Inferred: ${magenta(FormatScheme.formatSchemeWithOnlyEqualityConstraints(inferred))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      val newLineAndIndent: String = System.lineSeparator() + "  "

      def fmtTypeVar(tvar: Symbol.KindedTypeVarSym, declared: Boolean): String = {
        val color = if (declared) formatter.cyan _ else formatter.magenta _
        s"${color(FormatType.formatTypeVarSym(tvar))} of kind: '${FormatKind.formatKind(tvar.kind)}'."
      }

      def fmtQuantifiers(quantifiers: List[Symbol.KindedTypeVarSym], declared: Boolean): String = {
        if (quantifiers.isEmpty)
          "<< no type variables >>"
        else
          quantifiers.map(fmtTypeVar(_, declared)).mkString(newLineAndIndent)
      }

      s"""
         |The declared type variables:
         |  ${fmtQuantifiers(declared.quantifiers, declared = true)}
         |
         |The inferred type variables:
         |  ${fmtQuantifiers(inferred.quantifiers, declared = false)}
         |""".stripMargin
    })
  }

  /**
    * Effect Generalization Error.
    *
    * @param declared the declared effect.
    * @param inferred the inferred effect.
    * @param loc      the location where the error occurred.
    */
  case class EffectGeneralizationError(declared: Type, inferred: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"The inferred effect '${FormatEff.formatEff(inferred)}' cannot be generalized to '${FormatEff.formatEff(declared)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The inferred effect: '${red(FormatEff.formatEff(inferred))}' cannot be generalized to '${red(FormatEff.formatEff(declared))}'.
         |
         |${code(loc, "unable to generalize the effect.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Impure function declared as pure.
    *
    * @param loc the location where the error occurred.
    */
  case class ImpureDeclaredAsPure(loc: SourceLocation) extends TypeError {
    def summary: String = "Impure function declared as pure."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> ${red("Impure")} function declared as ${green("pure")}.
         |
         |${code(loc, "impure function.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """A function whose body is impure must be declared as so.
        |
        |For example:
        |
        |  def example(): Unit \ IO = println("hello")
        |                      ^^^^
        |""".stripMargin
    })
  }

  /**
    * Effectful function declared as pure.
    *
    * @param inferred the inferred effect.
    * @param loc      the location where the error occurred.
    */
  case class EffectfulDeclaredAsPure(inferred: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = "Effectful function declared as pure."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> ${red("Effectful")} function declared as ${green("pure")}.
         |
         |${code(loc, "effectful function.")}
         |
         |The function has the effect: ${FormatEff.formatEff(inferred)}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      """A function must declare all the effects used in its body.
        |
        |For example:
        |
        |  def example(f: Int32 -> Int32 \ ef): Int32 \ ef = f(123)
        |                                             ^^^^
        |""".stripMargin
    })
  }

  /**
    * Unexpected Type.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class UnexpectedType(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected Effect.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class UnexpectedEffect(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected type, but a checked type cast might work.
    *
    * @param expected the expected type.
    * @param inferred the inferred type.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred type.
    */
  case class PossibleCheckedTypeCast(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected type '${formatType(expected, Some(renv))}' but found type: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""
         |>> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |
         |'${formatType(expected, Some(renv))}' appears to be assignable from '${formatType(inferred, Some(renv))}'.
         |Consider using 'checked_cast'?
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""Flix does not support sub-typing nor sub-effecting.
         |
         |Nevertheless, 'checked_cast' is way to use sub-typing in a safe manner, for example:
         |
         |    let s = "Hello World";
         |    let o: ##java.lang.Object = checked_cast(s);
         |""".stripMargin
    )
  }

  /**
    * Unexpected effect, but a checked effect cast might work.
    *
    * @param expected the expected effect.
    * @param inferred the inferred effect.
    * @param renv     the rigidity environment.
    * @param loc      the location of the inferred effect.
    */
  case class PossibleCheckedEffectCast(expected: Type, inferred: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected effect '${formatType(expected, Some(renv))}' but found effect: '${formatType(inferred, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""
         |>> Expected effect: '${red(formatType(expected, Some(renv)))}' but found effect: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected effect.")}
         |
         |'${formatType(expected, Some(renv))}' appears to be a superset of '${formatType(inferred, Some(renv))}'.
         |Consider using 'checked_ecast'?
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""Flix does not support sub-typing nor sub-effecting.
         |
         |Nevertheless, 'checked_ecast' is way to use sub-effecting in a safe manner, for example:
         |
         |    let s = "pure expression";
         |    let o: Unit -> String \\ IO = () -> checked_ecast(s);
         |""".stripMargin
    )
  }

  /**
    * Mismatched Types.
    *
    * @param baseType1 the first base type.
    * @param baseType2 the second base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedTypes(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the types '${formatType(fullType1, Some(renv))}' and '${formatType(fullType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the types: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv))}
         |Type Two: ${formatType(fullType2, Some(renv))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Over-applied Function.
    *
    * @param excessArgument the type of the excess argument.
    * @param loc            the location where the error occurred.
    */
  case class OverApplied(excessArgument: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Over-applied function. Excess argument of type: '${formatType(excessArgument)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Over-applied function. Excess argument of type: '${red(formatType(excessArgument))}'.
         |
         |${code(loc, "over-applied function.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Under-applied Function.
    *
    * @param missingArgument the type of the missing argument.
    * @param loc             the location where the error occurred.
    */
  case class UnderApplied(missingArgument: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Under-applied function. Missing argument of type: '${formatType(missingArgument)}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Under-applied function. Missing argument of type: '${red(formatType(missingArgument))}'.
         |
         |${code(loc, "under-applied function.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Mismatched Boolean Formulas.
    *
    * @param baseType1 the first boolean formula.
    * @param baseType2 the second boolean formula.
    * @param fullType1 the first full type in which the first boolean formula occurs.
    * @param fullType2 the second full type in which the second boolean formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedBools(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the Boolean formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the Boolean formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched Boolean formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Mismatched Effect Formulas.
    *
    * @param baseType1 the first effect formula.
    * @param baseType2 the second effect formula.
    * @param fullType1 the first full type in which the first effect formula occurs.
    * @param fullType2 the second full type in which the second effect formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedEffects(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the effect formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the effect formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched effect formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Mismatched Case Set Formulas.
    *
    * @param baseType1 the first case set formula.
    * @param baseType2 the second case set formula.
    * @param fullType1 the first full type in which the first case set formula occurs.
    * @param fullType2 the second full type in which the second case set formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedCaseSets(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the case set formulas '${formatType(baseType1, Some(renv))}' and '${formatType(baseType2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the case set formulas: '${red(formatType(baseType1, Some(renv)))}' and '${red(formatType(baseType2, Some(renv)))}'.
         |
         |${code(loc, "mismatched case set formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Mismatched Pure and Effectful Arrows.
    *
    * @param baseType1 the first boolean formula.
    * @param baseType2 the second boolean formula.
    * @param fullType1 the first full type in which the first boolean formula occurs.
    * @param fullType2 the second full type in which the second boolean formula occurs.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class MismatchedArrowEffects(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Mismatched Pure and Effectful Functions."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Mismatched Pure and Effectful Functions.
         |
         |${code(loc, "mismatched pure and effectful functions.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv)))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Mismatched Arity.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MismatchedArity(tpe1: Type, tpe2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the types '${formatType(tpe1, Some(renv))}' and '${formatType(tpe2, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the types: '${red(formatType(tpe1, Some(renv)))}' and '${red(formatType(tpe2, Some(renv)))}'.
         |
         |${code(loc, "mismatched arity of types.")}
         |
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
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class OccursCheckError(baseVar: Type.Var, baseType: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the type variable '$baseVar' with the type '$baseType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unable to unify the type variable '${red(formatType(baseVar, Some(renv)))}' with the type '${red(formatType(baseType, Some(renv)))}'.
         |
         |>> The type variable occurs recursively within the type.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv))}
         |Type Two: ${formatType(fullType2, Some(renv))}
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
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedField(field: Name.Field, fieldType: Type, recordType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing field '$field' of type '$fieldType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Missing field '${red(field.name)}' of type '${cyan(formatType(fieldType, Some(renv)))}'.
         |
         |${code(loc, "missing field.")}
         |
         |The record type:
         |
         |  ${formatType(recordType, Some(renv))}
         |
         |does not contain the field '${red(field.name)}' of type ${cyan(formatType(fieldType, Some(renv)))}.
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
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedPredicate(pred: Name.Pred, predType: Type, schemaType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing predicate '${pred.name}' of type '$predType'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Missing predicate '${red(pred.name)}' of type '${cyan(formatType(predType, Some(renv)))}'.
         |
         |${code(loc, "missing predicate.")}
         |
         |The schema type:
         |
         |  ${formatType(schemaType, Some(renv))}
         |
         |does not contain the predicate '${red(pred.name)}' of type ${cyan(formatType(predType, Some(renv)))}.
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected non-record type error.
    *
    * @param tpe  the unexpected non-record type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class NonRecordType(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unexpected non-record type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected non-record type: '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, "unexpected non-record type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Unexpected non-schema type error.
    *
    * @param tpe  the unexpected non-schema type.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class NonSchemaType(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unexpected non-schema type '$tpe'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unexpected non-schema type: '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, "unexpected non-schema type.")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Missing type class instance for a function type.
    *
    * @param clazz the class of the instance.
    * @param tpe   the type of the instance.
    * @param renv  the rigidity environment.
    * @param loc   the location where the error occurred.
    */
  case class MissingArrowInstance(clazz: Symbol.ClassSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$clazz' class for the function type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> No instance of the '${cyan(clazz.toString)}' class for the ${magenta("function")} type '${red(formatType(tpe, Some(renv)))}'.
         |
         |>> Did you forget to apply the function to all of its arguments?
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Missing type class instance.
    *
    * @param clazz the class of the instance.
    * @param tpe   the type of the instance.
    * @param renv  the rigidity environment.
    * @param loc   the location where the error occurred.
    */
  case class MissingInstance(clazz: Symbol.ClassSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$clazz' class for the type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> No instance of the '${cyan(clazz.toString)}' class for the type '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Missing `Eq` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingEq(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Equality is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Eq."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Equality is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Eq.
         |
         |${code(loc, s"missing Eq instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To define equality on '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of Eq for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Eq for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Eq {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Missing `Order` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingOrder(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Order is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Order."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Order is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Order.
         |
         |${code(loc, s"missing Order instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To define an order on '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of Order for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Order for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Eq, Order {
         |    case Red, Green, Blue
         |  }
         |
         |Note: To derive Order you must also derive Eq.
         |""".stripMargin
    })
  }

  /**
    * Missing `ToString` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingToString(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"ToString is not defined for '${formatType(tpe, Some(renv))}'. Define or derive instance of ToString."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> ToString is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of ToString.
         |
         |${code(loc, s"missing ToString instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To define a string representation of '${formatType(tpe, Some(renv))}', either:
         |
         |  (a) define an instance of ToString for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of ToString for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with ToString {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Missing `Sendable` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingSendable(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Sendable is not defined for '${formatType(tpe, Some(renv))}'. Define or derive instance of Sendable."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Sendable is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Sendable.
         |
         |${code(loc, s"missing Sendable instance")}
         |
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      s"""To mark '${formatType(tpe, Some(renv))}' as sendable, either:
         |
         |  (a) define an instance of Sendable for '${formatType(tpe, Some(renv))}', or
         |  (b) use 'with' to derive an instance of Sendable for '${formatType(tpe, Some(renv))}', for example:.
         |
         |  enum Color with Sendable {
         |    case Red, Green, Blue
         |  }
         |
         |""".stripMargin
    })
  }

  /**
    * Unsupported equality error.
    *
    * @param econstr the unsupported equality constraint.
    * @param loc     the location where the error occurred.
    */
  case class UnsupportedEquality(econstr: BroadEqualityConstraint, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unsupported type equality: ${formatEqualityConstraint(econstr)}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unsupported type equality: ${formatEqualityConstraint(econstr)}
         |
         |${code(loc, "unsupported type equality.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Tip: Add an equality constraint to the function."
    })
  }

  /**
    * Irreducible associated type error
    *
    * @param sym the associated type symbol.
    * @param tpe the argument to the associated type
    * @param loc the location where the error occurred.
    */
  case class IrreducibleAssocType(sym: Symbol.AssocTypeSym, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    private val assocType: Type = Type.AssocType(Ast.AssocTypeConstructor(sym, SourceLocation.Unknown), tpe, Kind.Wild, SourceLocation.Unknown)
    def summary: String = s"Irreducible associated type: ${formatType(assocType)}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Irreducible associated type: ${formatType(assocType)}
         |
         |${code(loc, "irreducible associated type.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some({
      "Tip: Add an equality constraint to the function."
    })
  }

  /**
    * An error indicating that a region variable escapes its scope.
    *
    * @param rvar the region variable.
    * @param tpe  the type wherein the region variable escapes.
    * @param loc  the location where the error occurred.
    */
  case class RegionVarEscapes(rvar: Type.Var, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Region variable '${formatType(rvar)}' escapes its scope."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The region variable '${red(formatType(rvar))}' escapes its scope.
         |
         |${code(loc, "region variable escapes.")}
         |
         |The escaping expression has type:
         |
         |  ${red(formatType(tpe))}
         |
         |which contains the region variable.
         |
         |The region variable was declared here:
         |
         |${code(rvar.loc, "region variable declared here.")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error indicating an unexpected argument.
    *
    * @param sym      the symbol.
    * @param ith      the index of the unexpected argument.
    * @param expected the expected type.
    * @param actual   the actual type.
    * @param loc      the location where the error occurred.
    */
  case class UnexpectedArgument(sym: Symbol, ith: Int, expected: Type, actual: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'.
         |
         |${code(loc, s"expected: '${cyan(formatType(expected, Some(renv)))}'")}
         |
         |The function '${magenta(sym.toString)}' expects its ${Grammar.ordinal(ith)} argument to be of type '${formatType(expected, Some(renv))}'.
         |
         |Expected: ${formatType(expected, Some(renv))}
         |  Actual: ${formatType(actual, Some(renv))}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * An error indicating the number of effect operation arguments does not match the expected number.
    *
    * @param op       the effect operation symbol.
    * @param expected the expected number of arguments.
    * @param actual   the actual number of arguments.
    * @param loc      the location where the error occurred.
    */
  case class InvalidOpParamCount(op: Symbol.OpSym, expected: Int, actual: Int, loc: SourceLocation) extends TypeError {
    override def summary: String = s"Expected $expected parameter(s) but found $actual."

    /**
      * Returns the formatted error message.
      */
    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |
         |The operation $op expects $expected parameter(s),
         |but $actual are provided here.
         |
         |${code(loc, s"expected $expected parameter(s) but found $actual")}
         |""".stripMargin
    }

    /**
      * Returns a formatted string with helpful suggestions.
      */
    override def explain(formatter: Formatter): Option[String] = None
  }
}
