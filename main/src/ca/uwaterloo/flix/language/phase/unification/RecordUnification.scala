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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.Unification.unifyTypes
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import ca.uwaterloo.flix.util.Result.{Err, Ok}

object RecordUnification {

  /**
    * Performs unification on the given record rows in the given rigidity environment.
    *
    * The given types must have kind [[Kind.RecordRow]]
    */
  def unifyRows(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit univ: Ast.Multiverse, flix: Flix): Result[Substitution, UnificationError] = (tpe1, tpe2) match {

    case (tvar: Type.Var, tpe) => Unification.unifyVar(tvar, tpe, renv)

    case (tpe, tvar: Type.Var) => Unification.unifyVar(tvar, tpe, renv)

    case (Type.RecordRowEmpty, Type.RecordRowEmpty) => Ok(Substitution.empty)

    case (Type.RecordRowEmpty, _) => Err(UnificationError.MismatchedTypes(tpe1, tpe2))

    case (row1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(_), _), _, _), restRow1, _), row2) =>
      // Attempt to write the row to match.
      rewriteRecordRow(row2, row1, renv) flatMap {
        case (subst1, restRow2) =>
          unifyTypes(subst1(restRow1), subst1(restRow2), renv) flatMap {
            case subst2 => Result.Ok(subst2 @@ subst1)
          }
      }

    case _ => throw InternalCompilerException(s"unexpected types: ($tpe1), ($tpe2)", tpe1.loc)
  }

  /**
    * Attempts to rewrite the given row type `rewrittenRow` such that it shares a first label with `staticRow`.
    */
  private def rewriteRecordRow(rewrittenRow: Type, staticRow: Type, renv: RigidityEnv)(implicit univ: Ast.Multiverse, flix: Flix): Result[(Substitution, Type), UnificationError] = {

    def visit(row: Type): Result[(Substitution, Type), UnificationError] = (row, staticRow) match {
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field2), _), fieldType2, _), restRow2, loc),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), _), fieldType1, _), _, _)) =>
        // Case 1: The row is of the form { field2 :: fieldType2 | restRow2 }
        if (field1 == field2) {
          // Case 1.1: The fields match, their types must match.
          for {
            subst <- unifyTypes(fieldType1, fieldType2, renv)
          } yield (subst, restRow2)
        } else {
          // Case 1.2: The fields do not match, attempt to match with a field further down.
          visit(restRow2) map {
            case (subst, rewrittenRow) => (subst, Type.mkRecordRowExtend(field2, fieldType2, rewrittenRow, loc))
          }
        }
      case (tvar: Type.Var, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), _), fieldType1, _), _, _)) =>
        val tv = tvar
        // Case 2: The row is a type variable.
        if (staticRow.typeVars contains tv) {
          Err(UnificationError.OccursCheck(tv, staticRow))
        } else {
          // Introduce a fresh type variable to represent one more level of the row.
          val restRow2 = Type.freshVar(Kind.RecordRow, tvar.loc)
          val type2 = Type.mkRecordRowExtend(field1, fieldType1, restRow2, tvar.loc)
          val subst = Substitution.singleton(tv.sym, type2)
          Ok((subst, restRow2))
        }

      case (Type.Cst(TypeConstructor.RecordRowEmpty, _), Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field1), _), fieldType1, _), _, _)) =>
        // Case 3: The `field` does not exist in the record.
        Err(UnificationError.UndefinedField(field1, fieldType1, rewrittenRow))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonRecordType(rewrittenRow))
    }

    visit(rewrittenRow)
  }
}
