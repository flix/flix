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

import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, Symbol, Type, TypedAst}

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
    * A unification error due to an mismatch in type variable kinds.
    *
    * @param kind1 the first kind.
    * @param kind2 the second kind.
    */
  case class MismatchedKinds(kind1: Kind, kind2: Kind) extends UnificationError

  /**
    * A unification error resulting from a type constraint with no matching instance.
    *
    * @param clazz the class of the type constraint
    * @param tpe the type of the type constraint
    */
  case class NoMatchingInstance(clazz: Symbol.ClassSym, tpe: Type) extends UnificationError
}
