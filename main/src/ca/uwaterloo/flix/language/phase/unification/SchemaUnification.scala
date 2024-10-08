/*
 * Copyright 2022 Matthew Lutze
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.phase.unification.Unification.unifyTypes
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object SchemaUnification {

  /**
    * Performs unification on the given schema rows in the given rigidity environment.
    *
    * The given types must have kind [[Kind.SchemaRow]]
    */
  def unifyRows(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint], Boolean), UnificationError] = (tpe1, tpe2) match {

    case (tvar: Type.Var, tpe) => Unification.unifyVar(tvar, tpe, renv, eqEnv)

    case (tpe, tvar: Type.Var) => Unification.unifyVar(tvar, tpe, renv, eqEnv)

    case (Type.SchemaRowEmpty, Type.SchemaRowEmpty) => Ok((Substitution.empty, Nil, true))

    case (Type.SchemaRowEmpty, _) => Err(UnificationError.MismatchedTypes(tpe1, tpe2))

    case (row1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(_), _), _, _), restRow1, _), row2) =>
      // Attempt to write the row to match.
      rewriteSchemaRow(row2, row1, renv, eqEnv) flatMap {
        case (subst1, restRow2, econstrs1, prog1) =>
          unifyTypes(subst1(restRow1), subst1(restRow2), renv, eqEnv) flatMap {
            case (subst2, econstrs2, prog2) => Result.Ok((subst2 @@ subst1, econstrs1 ++ econstrs2, prog1 || prog2))
          }
      }

    case (Type.Cst(TypeConstructor.Error(_, _), _), _) => Ok((Substitution.empty, Nil, true))

    case (_, Type.Cst(TypeConstructor.Error(_, _), _)) => Ok((Substitution.empty, Nil, true))

    case _ => throw InternalCompilerException(s"unexpected types: ($tpe1), ($tpe2)", tpe1.loc)
  }

  /**
    * Attempts to rewrite the given row type `rewrittenRow` such that it shares a first label with `staticRow`.
    */
  // NOTE: This is a copy of the [[RecordUnification.rewriteRecordRow]] function. It would be nice if it could be the same function, but the shape of labels is different.
  private def rewriteSchemaRow(rewrittenRow: Type, staticRow: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[(Substitution, Type, List[Ast.BroadEqualityConstraint], Boolean), UnificationError] = {

    def visit(row: Type): Result[(Substitution, Type, List[Ast.BroadEqualityConstraint], Boolean), UnificationError] = (row, staticRow) match {
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label2), _), fieldType2, _), restRow2, loc),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label1), _), fieldType1, _), _, _)) =>
        // Case 1: The row is of the form { label2 :: fieldType2 | restRow2 }
        if (label1 == label2) {
          // Case 1.1: The labels match, their types must match.
          for {
            (subst, econstrs, prog) <- unifyTypes(fieldType1, fieldType2, renv, eqEnv)
          } yield (subst, restRow2, econstrs, prog)
        } else {
          // Case 1.2: The labels do not match, attempt to match with a label further down.
          visit(restRow2) map {
            case (subst, rewrittenRow, econstrs, prog) => (subst, Type.mkSchemaRowExtend(label2, fieldType2, rewrittenRow, loc), econstrs, prog)
          }
        }
      case (tvar: Type.Var, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label1), _), fieldType1, _), _, _)) =>
        val tv = tvar
        // Case 2: The row is a type variable.
        if (staticRow.typeVars contains tv) {
          Err(UnificationError.OccursCheck(tv, staticRow))
        } else {
          // Introduce a fresh type variable to represent one more level of the row.
          // We use the same level as the type variable to maintain consistency.
          val restRow2 = Type.freshVar(Kind.SchemaRow, tvar.loc)
          val type2 = Type.mkSchemaRowExtend(label1, fieldType1, restRow2, tvar.loc)
          val subst = Substitution.singleton(tv.sym, type2)
          Ok((subst, restRow2, Nil, true))
        }

      case (Type.Cst(TypeConstructor.SchemaRowEmpty, _), Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label1), _), fieldType1, _), _, _)) =>
        // Case 3: The `label` does not exist in the record.
        Err(UnificationError.UndefinedPredicate(label1, fieldType1, rewrittenRow))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonSchemaType(rewrittenRow))
    }

    visit(rewrittenRow)
  }

  // MATT
  def withProgress(pair: (Substitution, List[Ast.BroadEqualityConstraint])): (Substitution, List[Ast.BroadEqualityConstraint], Boolean) = pair match {
    case (subst, constrs) => (subst, constrs, true)
  }
}
