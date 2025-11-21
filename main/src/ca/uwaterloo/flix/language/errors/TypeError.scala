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
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.shared.SymbolSet
import ca.uwaterloo.flix.language.fmt.FormatEqualityConstraint.formatEqualityConstraint
import ca.uwaterloo.flix.language.ast.shared.Denotation
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.util.{Formatter, Grammar}

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.TypeError
}

object TypeError {

  /**
    * Java constructor not found type error.
    *
    * @param tpes the types of the arguments.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occured.
    */
  case class ConstructorNotFound(clazz: Class[?], tpes: List[Type], renv: RigidityEnv, loc: SourceLocation) extends TypeError {
    def summary: String = s"Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Java '${clazz.getName}' constructor with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Java '${clazz.getName}' constructor not found")}
         |""".stripMargin
    }
  }

  /**
    * Java method not found type error.
    *
    * @param tpe  the type of the receiver object.
    * @param tpes the types of the arguments.
    * @param loc  the location where the error occurred.
    */
  case class MethodNotFound(methodName: Name.Ident, tpe: Type, tpes: List[Type], loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Java method '$methodName' in type '$tpe' with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Java method '$methodName' from type '${red(formatType(tpe, None))}' with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Java method '$methodName' not found")}
         |""".stripMargin
    }
  }

  /**
    * Java field not found type error.
    *
    * @param base the source location of the receiver expression.
    * @param tpe  the type of the receiver object.
    * @param loc  the location where the error occurred.
    */
  case class FieldNotFound(base: SourceLocation, fieldName: Name.Ident, tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Java field '$fieldName' in type '$tpe' not found."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Java field '$fieldName' from type '${red(formatType(tpe, None))}' not found.
         |
         |${code(loc, s"Java field '$fieldName' not found")}
         |""".stripMargin
    }
  }

  /**
    * Static Java method not found type error.
    *
    * @param clazz the Java class expected to contain the static method
    * @param tpes  the types of the arguments.
    * @param renv  the rigidity environment.
    * @param loc   the location where the error occurred.
    */
  case class StaticMethodNotFound(clazz: Class[?], methodName: Name.Ident, tpes: List[Type], renv: RigidityEnv, loc: SourceLocation) extends TypeError {
    def summary: String = s"Static Java method '$methodName' from class ${clazz.getName} with arguments types (${tpes.mkString(", ")}) not found."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Static Java method '$methodName' from class '${clazz.getName}' with arguments types (${tpes.mkString(", ")}) not found.
         |
         |${code(loc, s"Static Java method '$methodName' not found")}
         |""".stripMargin
    }
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
      import formatter.*
      s""">> Unable to unify the effect formulas: '${red(formatType(baseType1, Some(renv), minimizeEffs = true))}' and '${red(formatType(baseType2, Some(renv), minimizeEffs = true))}'.
         |
         |${code(loc, "mismatched effect formulas.")}
         |
         |Type One: ${cyan(formatType(fullType1, Some(renv)))}
         |Type Two: ${magenta(formatType(fullType2, Some(renv), minimizeEffs = true))}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Predicate Arity.
    *
    * @param pred   the predicate label.
    * @param arity1 the first arity.
    * @param arity2 the second arity.
    * @param loc1   the location where the predicate is used with the first arity.
    * @param loc2   the location where the predicate is used with the second arity.
    * @param loc    the location where the unification error occurred.
    */
  case class MismatchedPredicateArity(pred: Name.Pred, arity1: Int, arity2: Int, loc1: SourceLocation, loc2: SourceLocation, loc: SourceLocation) extends TypeError {
    def summary: String = s"Mismatched predicate arity: '${pred.name}/$arity1' and '${pred.name}/$arity2'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Mismatched predicate arity: '${cyan(pred.name)}/$arity1' and '${cyan(pred.name)}/$arity2'.
         |
         |${code(loc1, s"here '${pred.name}' has arity $arity1.")}
         |
         |${code(loc2, s"here '${pred.name}' has arity $arity2.")}
         |""".stripMargin
    }
  }

  /**
    * Mismatched Predicate Denotation.
    *
    * @param pred the predicate label.
    * @param den1 the first denotation.
    * @param den2 the second denotation.
    * @param loc1 the location where the predicate is used with the first denotation.
    * @param loc2 the location where the predicate is used with the second denotation.
    * @param loc  the location where the unification error occurred.
    */
  case class MismatchedPredicateDenotation(pred: Name.Pred, den1: Denotation, den2: Denotation, loc1: SourceLocation, loc2: SourceLocation, loc: SourceLocation) extends TypeError {
    def summary: String = s"Mismatched predicate denotation for '${pred.name}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      def pretty(den: Denotation): String = den match {
        case Denotation.Relational => "relation"
        case Denotation.Latticenal => "lattice"
      }
      s""">> Mismatched predicate denotation for '${cyan(pred.name)}'.
         |
         |${code(loc1, s"here '${pred.name}' is a ${magenta(pretty(den1))}.")}
         |
         |${code(loc2, s"here '${pred.name}' is a ${magenta(pretty(den2))}.")}
         |""".stripMargin
    }
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
    def amb: SymbolSet = SymbolSet.ambiguous(SymbolSet.symbolsOf(fullType1), SymbolSet.symbolsOf(fullType2))
    def summary: String = s"Unable to unify the types '${formatType(fullType1, Some(renv), minimizeEffs = true, amb = amb)}' and '${formatType(fullType2, Some(renv), minimizeEffs = true, amb = amb)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unable to unify the types: '${red(formatType(baseType1, Some(renv), minimizeEffs = true, amb = amb))}' and '${red(formatType(baseType2, Some(renv), minimizeEffs = true, amb = amb))}'.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv), minimizeEffs = true, amb = amb)}
         |Type Two: ${formatType(fullType2, Some(renv), minimizeEffs = true, amb = amb)}
         |""".stripMargin
    }
  }

  /**
    * Missing trait instance.
    *
    * @param trt  the trait of the instance.
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstance(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$trt' trait for the type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> No instance of the '${cyan(trt.toString)}' trait for the type '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }
  }

  /**
    * Missing trait instance for a function type.
    *
    * @param trt  the trait of the instance.
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceArrow(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No instance of the '$trt' trait for the function type '${formatType(tpe, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> No instance of the '${cyan(trt.toString)}' trait for the ${magenta("function")} type '${red(formatType(tpe, Some(renv)))}'.
         |
         |>> Did you forget to apply the function to all of its arguments?
         |
         |${code(loc, s"missing instance")}
         |
         |""".stripMargin
    }
  }

  /**
    * Missing `Eq` instance.
    *
    * @param tpe  the type of the instance.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingInstanceEq(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Equality is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Eq."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Equality is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Eq.
         |
         |${code(loc, s"missing Eq instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
  case class MissingInstanceOrder(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Order is not defined on '${formatType(tpe, Some(renv))}'. Define or derive instance of Order."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Order is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of Order.
         |
         |${code(loc, s"missing Order instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
  case class MissingInstanceToString(tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"ToString is not defined for '${formatType(tpe, Some(renv))}'. Define or derive instance of ToString."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> ToString is not defined on ${red(formatType(tpe, Some(renv)))}. Define or derive an instance of ToString.
         |
         |${code(loc, s"missing ToString instance")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
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
    * Occurs Check.
    *
    * @param baseVar   the base type variable.
    * @param baseType  the base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param renv      the rigidity environment.
    * @param loc       the location where the error occurred.
    */
  case class OccursCheck(baseVar: Type.Var, baseType: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Unable to unify the type variable '$baseVar' with the type '$baseType'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Unable to unify the type variable '${red(formatType(baseVar, Some(renv)))}' with the type '${red(formatType(baseType, Some(renv)))}'.
         |
         |>> The type variable occurs recursively within the type.
         |
         |${code(loc, "mismatched types.")}
         |
         |Type One: ${formatType(fullType1, Some(renv))}
         |Type Two: ${formatType(fullType2, Some(renv))}
         |""".stripMargin
    }
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
      import formatter.*
      s""">> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |
         |'${formatType(expected, Some(renv))}' appears to be assignable from '${formatType(inferred, Some(renv))}'.
         |Consider using 'checked_cast'?
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some(
      s"""Flix does not support sub-typing nor sub-effecting.
         |
         |Nevertheless, 'checked_cast' is way to use sub-typing in a safe manner, for example:
         |
         |    let s = "Hello World";
         |    let o: Object = checked_cast(s);
         |""".stripMargin
    )
  }

  /**
    * A unification equation system was too complex to solve.
    *
    * @param loc the location where the error occurred.
    */
  case class TooComplex(msg: String, loc: SourceLocation) extends TypeError {
    def summary: String = s"Type inference too complex: $msg"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> ${red(s"Type inference failed due to too complex unification: $msg")}'.
         |
         |Try to break your function into smaller functions.
         |
         |${code(loc, "too complex constraints")}
         |
         |""".stripMargin
    }
  }

  /**
    * Undefined label error.
    *
    * @param label      the name of the missing label.
    * @param labelType  the type of the missing label.
    * @param recordType the record type where the label is missing.
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class UndefinedLabel(label: Name.Label, labelType: Type, recordType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing label '$label' of type '$labelType'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing label '${red(label.name)}' of type '${cyan(formatType(labelType, Some(renv)))}'.
         |
         |${code(loc, "missing label.")}
         |
         |The record type:
         |
         |  ${formatType(recordType, Some(renv))}
         |
         |does not contain the label '${red(label.name)}' of type ${cyan(formatType(labelType, Some(renv)))}.
         |""".stripMargin
    }
  }

  /**
    * Extra label error.
    *
    * @param label      the name of the extra label.
    * @param labelType  the type of the extra label.
    * @param recordType the record type where the label is missing.
    * @param renv       the rigidity environment.
    * @param loc        the location where the error occurred.
    */
  case class ExtraLabel(label: Name.Label, labelType: Type, recordType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Extra label '$label' of type '$labelType'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Extra label '${red(label.name)}' of type '${cyan(formatType(labelType, Some(renv)))}'.
         |
         |${code(loc, "extra label.")}
         |
         |The record type:
         |
         |  ${formatType(recordType, Some(renv))}
         |
         |contains the extra label '${red(label.name)}' of type ${cyan(formatType(labelType, Some(renv)))}.
         |""".stripMargin
    }
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
  case class UndefinedPred(pred: Name.Pred, predType: Type, schemaType: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Missing predicate '${pred.name}' of type '$predType'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Missing predicate '${red(pred.name)}' of type '${cyan(formatType(predType, Some(renv)))}'.
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
  case class UnexpectedArg(sym: Symbol, ith: Int, expected: Type, actual: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Expected argument of type '${formatType(expected, Some(renv))}', but got '${formatType(actual, Some(renv))}'.
         |
         |${code(loc, s"expected: '${cyan(formatType(expected, Some(renv)))}'")}
         |
         |The function '${magenta(sym.toString)}' expects its ${Grammar.ordinal(ith)} argument to be of type '${formatType(expected, Some(renv))}'.
         |
         |Expected: ${formatType(expected, Some(renv))}
         |  Actual: ${formatType(actual, Some(renv))}
         |""".stripMargin
    }
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
      import formatter.*
      s""">> Expected type: '${red(formatType(expected, Some(renv)))}' but found type: '${red(formatType(inferred, Some(renv)))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }
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
    def amb: SymbolSet = SymbolSet.ambiguous(SymbolSet.symbolsOf(expected), SymbolSet.symbolsOf(inferred))

    def summary: String = s"Expected type '${formatType(expected, Some(renv), amb = amb)}' but found type: '${formatType(inferred, Some(renv), amb = amb)}'."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Expected type: '${red(formatType(expected, Some(renv), amb = amb))}' but found type: '${red(formatType(inferred, Some(renv), amb = amb))}'.
         |
         |${code(loc, "expression has unexpected type.")}
         |""".stripMargin
    }
  }

  /**
    * Unresolved constructor type error.
    * This is a dummy error used in Java constructor type reconstruction for InvokeConstructor.
    */
  case class UnresolvedConstructor(loc: SourceLocation) extends TypeError {
    def summary: String = s"Unresolved constructor"

    def message(formatter: Formatter): String = s"Unresolved constructor"
  }

  /**
    * Unresolved field type error.
    * This is a dummy error used in Java field type reconstruction for GetField.
    */
  case class UnresolvedField(loc: SourceLocation) extends TypeError {
    def summary: String = s"Unresolved field"

    def message(formatter: Formatter): String = s"Unresolved field"
  }

  /**
    * Unresolved method type error.
    * This is a dummy error used in Java method type reconstruction for InvokeMethod.
    */
  case class UnresolvedMethod(loc: SourceLocation) extends TypeError {
    def summary: String = s"Unresolved method"

    def message(formatter: Formatter): String = s"Unresolved method"
  }

  /**
    * Unresolved method type error.
    * This is a dummy error used in Java method type reconstruction for InvokeStaticMethod.
    */
  case class UnresolvedStaticMethod(loc: SourceLocation) extends TypeError {
    def summary: String = s"Unresolved static method"

    def message(formatter: Formatter): String = s"Unresolved static method"
  }

  /**
    * Missing trait constraint.
    *
    * @param trt  the trait of the constraint.
    * @param tpe  the type of the constraint.
    * @param renv the rigidity environment.
    * @param loc  the location where the error occurred.
    */
  case class MissingTraitConstraint(trt: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = s"No constraint of the '$trt' trait for the type '${formatType(tpe, Some(renv))}'"

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> No constraint of the '${cyan(trt.toString)}' trait for the type '${red(formatType(tpe, Some(renv)))}'.
         |
         |${code(loc, s"missing constraint")}
         |
         |""".stripMargin
    }
  }

  /**
    * Non-unit type used in statement position.
    *
    * @param tpe the actual type.
    * @param loc the location where the error occurred.
    */
  case class NonUnitStatement(tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends TypeError {
    def summary: String = "Non-unit type used in statement position."

    def message(formatter: Formatter): String = {
      import formatter.*
      s""">> Statement has non-unit type: ${FormatType.formatType(tpe)}.
         |
         |${code(loc, s"non-unit type")}
         |
         |""".stripMargin
    }
  }

}
