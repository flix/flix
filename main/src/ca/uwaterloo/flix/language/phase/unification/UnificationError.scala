/*
 *  Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol, Type}

/**
  * A common super-type for unification errors.
  */
sealed trait UnificationError

object UnificationError {

  /**
    * An unification error due to a mismatch between the types `tpe1` and `tpe2`.
    *
    * @param tpe1 the first type.
    * @param tpe2 the second type.
    */
  case class MismatchedTypes(tpe1: Type, tpe2: Type) extends UnificationError

  /**
    * An unification error due to a mismatch between the boolean formulas `tpe1` and `tpe2`.
    *
    * @param tpe1 the first boolean formula.
    * @param tpe2 the second boolean formula.
    */
  case class MismatchedBools(tpe1: Type, tpe2: Type) extends UnificationError

  /**
    * An unification error due to a mismatch between the effect formulas `tpe1` and `tpe2`.
    *
    * @param tpe1 the first effect formula.
    * @param tpe2 the second effect formula.
    */
  case class MismatchedEffects(tpe1: Type, tpe2: Type) extends UnificationError

  /**
    * An unification error due to a mismatch between the case set formulas `tpe1` and `tpe2`.
    *
    * @param tpe1 the first case set formula.
    * @param tpe2 the second case set formula.
    */
  case class MismatchedCaseSets(tpe1: Type, tpe2: Type) extends UnificationError

  /**
    * An unification error due to a mismatch between the arity of `ts1` and `ts2`.
    *
    * @param ts1 the first list of types.
    * @param ts2 the second list of types.
    */
  case class MismatchedArity(ts1: List[Type], ts2: List[Type]) extends UnificationError

  /**
    * An unification error due to a rigid type variable `tvar` in `tpe`.
    *
    * @param tvar the type variable.
    * @param tpe  the type.
    */
  case class RigidVar(tvar: Type.Var, tpe: Type) extends UnificationError

  /**
    * An unification error due to an occurrence of `tvar` in `tpe`.
    *
    * @param tvar the type variable.
    * @param tpe  the type.
    */
  case class OccursCheck(tvar: Type.Var, tpe: Type) extends UnificationError

  /**
    * An unification error due the field `fieldName` of type `fieldType` missing from the type `recordType`.
    *
    * @param field      the name of the missing field.
    * @param fieldType  the type of the missing field.
    * @param recordType the record type where the field is missing.
    */
  case class UndefinedField(field: Name.Field, fieldType: Type, recordType: Type) extends UnificationError

  /**
    * An unification error due the predicate `pred` of type `predType` missing from the type `schemaType`.
    *
    * @param pred       the name of the missing predicate.
    * @param predType   the type of the missing predicate.
    * @param schemaType the schema type where the predicate is missing.
    */
  case class UndefinedPredicate(pred: Name.Pred, predType: Type, schemaType: Type) extends UnificationError

  /**
    * An unification error due to an unexpected non-record type.
    *
    * @param nonRecordType the unexpected non-record type.
    */
  case class NonRecordType(nonRecordType: Type) extends UnificationError

  /**
    * An unification error due to an unexpected non-schema type.
    *
    * @param nonSchemaType the unexpected non-schema type.
    */
  case class NonSchemaType(nonSchemaType: Type) extends UnificationError

  /**
    * A unification error resulting from a type constraint with no matching instance.
    *
    * @param tconstr the type constraint.
    */
  case class NoMatchingInstance(tconstr: Ast.TypeConstraint) extends UnificationError

  /**
    * A unification error resulting from multiple matching instances.
    *
    * @param tconstr the type constraint.
    */
  case class MultipleMatchingInstances(tconstr: Ast.TypeConstraint) extends UnificationError

  /**
    * A unification error resulting from an equality constraint that is not supported by the context.
    *
    * @param t1 the first type
    * @param t2 the second type
    */
  case class UnsupportedEquality(t1: Type, t2: Type) extends UnificationError

  /**
    * A unification error resulting from an associated type expression that cannot be reduced.
    *
    * @param sym the associated type symbol
    * @param t   the type
    */
  case class IrreducibleAssocType(sym: Symbol.AssocTypeSym, t: Type) extends UnificationError

}
