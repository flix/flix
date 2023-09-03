/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object TypeNormalization {


  /**
    * Converts a type (with the below assumptions) into an equivalent type in
    * normalized form, which will be the same for all other equivalent types.
    *
    * Returns a type where
    * 1. Formulas in types have been fully evaluated (and ordered in the case of
    *    sets)
    * 2. Types involving rows have been sorted alphabetically (respecting
    *    duplicate label ordering)
    * 3. The assumptions still hold
    *
    * Assumes that
    * 1. `tpe` is ground (no type variables)
    * 2. `tpe` has no aliases
    * 3. `tpe` has no associated types
    */
  def normalizeType(tpe: Type): Type = tpe match {
    case Type.Var(sym, loc) =>
      throw InternalCompilerException(s"Unexpected type var '$sym'", loc)
    case Type.Cst(_, _) =>
      tpe
    case Type.Apply(tpe1, tpe2, applyLoc) =>
      val t1 = normalizeType(tpe1)
      val t2 = normalizeType(tpe2)
      t1 match {
        // Simplify effect set formulas.
        case Type.Cst(TypeConstructor.Complement, _) => t2 match {
          case Type.Pure => Type.EffUniv
          case Type.EffUniv => Type.Pure
          case _ => throw InternalCompilerException(s"Unexpected non-simple effect: $tpe", applyLoc)
        }
        case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _) =>
          (x, t2) match {
            case (Type.Pure, Type.Pure) => Type.Pure
            case (Type.Pure, Type.EffUniv) => Type.EffUniv
            case (Type.EffUniv, Type.Pure) => Type.EffUniv
            case (Type.EffUniv, Type.EffUniv) => Type.EffUniv
            case _ => throw InternalCompilerException(s"Unexpected non-simple effect: $tpe", applyLoc)
          }
        case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _) =>
          (x, t2) match {
            case (Type.Pure, Type.Pure) => Type.Pure
            case (Type.Pure, Type.EffUniv) => Type.Pure
            case (Type.EffUniv, Type.Pure) => Type.Pure
            case (Type.EffUniv, Type.EffUniv) => Type.EffUniv
            case _ => throw InternalCompilerException(s"Unexpected non-simple effect: $tpe", applyLoc)
          }

        // Simplify boolean formulas.
        case Type.Cst(TypeConstructor.Not, _) => t2 match {
          case Type.True => Type.False
          case Type.False => Type.True
          case _ => throw InternalCompilerException(s"Unexpected non-simple Boolean formula: $tpe", applyLoc)
        }
        case Type.Apply(Type.Cst(TypeConstructor.And, _), x, _) =>
          (x, t2) match {
            case (Type.True, Type.True) => Type.True
            case (Type.True, Type.False) => Type.False
            case (Type.False, Type.True) => Type.False
            case (Type.False, Type.False) => Type.False
            case _ => throw InternalCompilerException(s"Unexpected non-simple Boolean formula: $tpe", applyLoc)
          }
        case Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _) =>
          (x, t2) match {
            case (Type.True, Type.True) => Type.True
            case (Type.True, Type.False) => Type.True
            case (Type.False, Type.True) => Type.True
            case (Type.False, Type.False) => Type.False
            case _ => throw InternalCompilerException(s"Unexpected non-simple Boolean formula: $tpe", applyLoc)
          }

        // Simplify case set formulas
        case Type.Cst(TypeConstructor.CaseComplement(enumSym), _) => t2 match {
          case Type.Cst(TypeConstructor.CaseSet(syms, _), loc) =>
            Type.Cst(TypeConstructor.CaseSet(enumSym.universe.diff(syms), enumSym), loc)
          case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula: $tpe", applyLoc)
        }
        case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(enumSym), _), x, loc) =>
          (x, t2) match {
            case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
              Type.Cst(TypeConstructor.CaseSet(syms1.intersect(syms2), enumSym), loc)
            case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula: $tpe", applyLoc)
          }
        case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(enumSym), _), x, loc) =>
          (x, t2) match {
            case (Type.Cst(TypeConstructor.CaseSet(syms1, _), _), Type.Cst(TypeConstructor.CaseSet(syms2, _), _)) =>
              Type.Cst(TypeConstructor.CaseSet(syms1.union(syms2), enumSym), loc)
            case _ => throw InternalCompilerException(s"Unexpected non-simple case set formula: $tpe", applyLoc)
          }

        // Sort record row fields
        case Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), labelType, _) =>
          insertRecordLabel(label, labelType, t2, applyLoc)

        // Sort schema row fields
        case Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), predType, _) =>
          insertSchemaPred(pred, predType, t2, applyLoc)

        // Else just apply
        case x => Type.Apply(x, t2, applyLoc)
      }
    case Type.Alias(cst, _, _, loc) =>
      throw InternalCompilerException(s"Unexpected type alias: '${cst.sym}'", loc)
    case Type.AssocType(cst, _, _, loc) =>
      throw InternalCompilerException(s"Unexpected associated type: '${cst.sym}'", loc)
  }

  /**
    * Inserts the given label into `rest` in its ordered position, assuming that
    * `rest` is already ordered. This, together with [[normalizeType]]
    * effectively implements insertion sort.
    */
  private def insertRecordLabel(label: Name.Field, labelType: Type, rest: Type, loc: SourceLocation): Type = rest match {
    // empty rest, create the singleton record row
    case Type.Cst(TypeConstructor.RecordRowEmpty, emptyLoc) =>
      Type.mkRecordRowExtend(label, labelType, Type.mkRecordRowEmpty(emptyLoc), loc)
    // the current field should be before the next field and since
    // - we insert from the left, one by one
    // - rest is ordered
    // we can return the current field with the rest
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), _), _, _), _, _) if label.name <= field1.name =>
      Type.mkRecordRowExtend(label, labelType, rest, loc)
    // The current field should be after the next field, so we swap and continue recursively
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label1), field1Loc), field1Type, field1TypeLoc), rest1, rest1Loc) =>
      val tail = insertRecordLabel(label, labelType, rest1, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label1), field1Loc), field1Type, field1TypeLoc), tail, rest1Loc)
    case other => throw InternalCompilerException(s"Unexpected record rest: '$other'", rest.loc)
  }

  /**
    * Inserts the given predicate into `rest` in its ordered position, assuming that
    * `rest` is already ordered. This, together with [[normalizeType]]
    * effectively implements insertion sort.
    */
  private def insertSchemaPred(pred: Name.Pred, predType: Type, rest: Type, loc: SourceLocation): Type = rest match {
    // empty rest, create the singleton schema row
    case Type.Cst(TypeConstructor.SchemaRowEmpty, _) =>
      Type.mkSchemaRowExtend(pred, predType, rest, loc)
    // the current pred should be before the next pred and since
    // - we insert from the left, one by one
    // - rest is ordered
    // we can return the current pred with the rest
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), _), _, _), _, _) if pred.name <= pred1.name =>
      Type.mkSchemaRowExtend(pred, predType, rest, loc)
    // The current pred should be after the next pred, so we swap and continue recursively
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), pred1Loc), pred1Type, pred1TypeLoc), rest1, rest1Loc) =>
      val rest2 = insertSchemaPred(pred, predType, rest1, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred1), pred1Loc), pred1Type, pred1TypeLoc), rest2, rest1Loc)
    case other =>
      throw InternalCompilerException(s"Unexpected schema rest: '$other'", rest.loc)
  }


}
