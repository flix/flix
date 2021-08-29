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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object Unification {

  /**
    * Unify the two type variables `x` and `y`.
    */
  private def unifyVars(x: Type.KindedVar, y: Type.KindedVar)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Case 0: types are identical
    if (x.id == y.id) {
      Result.Ok(Substitution.empty)
    } else {
      (x.rigidity, y.rigidity) match {
        // Case 1: x is flexible and a superkind of y
        case (Rigidity.Flexible, _) if y.kind <:: x.kind => Result.Ok(Substitution.singleton(x, y))
        // Case 2: y is flexible and a superkind of x
        case (_, Rigidity.Flexible) if x.kind <:: y.kind => Result.Ok(Substitution.singleton(y, x))
        // Case 3: both variables are rigid
        case (Rigidity.Rigid, Rigidity.Rigid) => Result.Err(UnificationError.RigidVar(x, y))
        // Case 4: at least one variable is flexible but not a superkind of the other
        case _ => Result.Err(UnificationError.MismatchedKinds(x.kind, y.kind))
      }
    }
  }

  /**
    * Unifies the given variable `x` with the given non-variable type `tpe`.
    */
  private def unifyVar(x: Type.KindedVar, tpe: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // NB: The `tpe` type must be a non-var.
    if (tpe.isInstanceOf[Type.Var])
      throw InternalCompilerException(s"Unexpected variable type: '$tpe'.")

    // Check if `x` is rigid.
    if (x.rigidity == Rigidity.Rigid) {
      return Result.Err(UnificationError.RigidVar(x, tpe))
    }

    // Check if `x` occurs within `tpe`.
    if (tpe.typeVars contains x) {
      return Result.Err(UnificationError.OccursCheck(x, tpe))
    }

    // Check if the kind of `x` matches the kind of `tpe`.
    if (!(tpe.kind <:: x.kind)) {
      return Result.Err(UnificationError.MismatchedKinds(x.kind, tpe.kind))
    }

    Result.Ok(Substitution.singleton(x, tpe))
  }


  /**
    * Unifies the two given types `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def unifyTypes(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    (tpe1, tpe2) match {
      case (x: Type.Var, y: Type.Var) => unifyVars(x.asKinded, y.asKinded)

      case (x: Type.Var, _) =>
        if (x.kind == Kind.Bool || tpe2.kind == Kind.Bool)
          BoolUnification.unify(x, tpe2)
        else
          unifyVar(x.asKinded, tpe2)

      case (_, x: Type.Var) =>
        if (x.kind == Kind.Bool || tpe1.kind == Kind.Bool)
          BoolUnification.unify(x, tpe1)
        else
          unifyVar(x.asKinded, tpe1)

      case (Type.Cst(c1, _), Type.Cst(c2, _)) if c1 == c2 => Result.Ok(Substitution.empty)

      case _ if tpe1.kind == Kind.Bool || tpe2.kind == Kind.Bool =>
        BoolUnification.unify(tpe1, tpe2)

      case (row1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(_), _), _, _), restRow1, _), row2) =>
        // Attempt to write the row to match.
        rewriteRecordRow(row2, row1) flatMap {
          case (subst1, restRow2) =>
            unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
              case subst2 => Result.Ok(subst2 @@ subst1)
            }
        }

      case (row1@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(_), _), _, _), restRow1, _), row2) =>
        // Attempt to write the row to match.
        rewriteSchemaRow(row2, row1) flatMap {
          case (subst1, restRow2) =>
            unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
              case subst2 => Result.Ok(subst2 @@ subst1)
            }
        }

      case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) =>
        unifyTypes(t11, t21) match {
          case Result.Ok(subst1) => unifyTypes(subst1(t12), subst1(t22)) match {
            case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
            case Result.Err(e) => Result.Err(e)
          }
          case Result.Err(e) => Result.Err(e)
        }

      case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
    }
  }

  /**
    * Attempts to rewrite the given row type `rewrittenRow` such that it shares a first label with `staticRow`.
    */
  private def rewriteRecordRow(rewrittenRow: Type, staticRow: Type)(implicit flix: Flix): Result[(Substitution, Type), UnificationError] = {

    def visit(row: Type): Result[(Substitution, Type), UnificationError] = (row, staticRow) match {
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field2), _), fieldType2, _), restRow2, loc),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field1), _), fieldType1, _), _, _)) =>
        // Case 1: The row is of the form %{ field2: fieldType2 | restRow2 }
        if (field1 == field2) {
          // Case 1.1: The fields match, their types must match.
          for {
            subst <- unifyTypes(fieldType1, fieldType2)
          } yield (subst, restRow2)
        } else {
          // Case 1.2: The fields do not match, attempt to match with a field further down.
          visit(restRow2) map {
            case (subst, rewrittenRow) => (subst, Type.mkRecordExtend(field2, fieldType2, rewrittenRow, loc))
          }
        }
      case (tvar: Type.Var, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field1), _), fieldType1, _), _, _)) =>
        val tv = tvar.asKinded
        // Case 2: The row is a type variable.
        if (staticRow.typeVars contains tv) {
          Err(UnificationError.OccursCheck(tv, staticRow))
        } else {
          // Introduce a fresh type variable to represent one more level of the row.
          val restRow2 = Type.freshVar(Kind.Record, tvar.loc)
          val type2 = Type.mkRecordExtend(field1, fieldType1, restRow2, tvar.loc)
          val subst = Substitution.singleton(tv, type2)
          Ok((subst, restRow2))
        }

      case (Type.Cst(TypeConstructor.RecordEmpty, _), Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field1), _), fieldType1, _), _, _)) =>
        // Case 3: The `field` does not exist in the record.
        Err(UnificationError.UndefinedField(field1, fieldType1, rewrittenRow))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonRecordType(rewrittenRow))
    }

    visit(rewrittenRow)
  }

  /**
    * Attempts to rewrite the given row type `rewrittenRow` such that it shares a first label with `staticRow`.
    */
  // TODO: This is a copy of the above function. It would be nice if it could be the same function, but the shape of labels is different.
  private def rewriteSchemaRow(rewrittenRow: Type, staticRow: Type)(implicit flix: Flix): Result[(Substitution, Type), UnificationError] = {

    def visit(row: Type): Result[(Substitution, Type), UnificationError] = (row, staticRow) match {
      case (Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label2), _), fieldType2, _), restRow2, loc),
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label1), _), fieldType1, _), _, _)) =>
        // Case 1: The row is of the form %{ label2: fieldType2 | restRow2 }
        if (label1 == label2) {
          // Case 1.1: The labels match, their types must match.
          for {
            subst <- unifyTypes(fieldType1, fieldType2)
          } yield (subst, restRow2)
        } else {
          // Case 1.2: The labels do not match, attempt to match with a label further down.
          visit(restRow2) map {
            case (subst, rewrittenRow) => (subst, Type.mkSchemaExtend(label2, fieldType2, rewrittenRow, loc))
          }
        }
      case (tvar: Type.Var, Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label1), _), fieldType1, _), _, _)) =>
        val tv = tvar.asKinded
        // Case 2: The row is a type variable.
        if (staticRow.typeVars contains tv) {
          Err(UnificationError.OccursCheck(tv, staticRow))
        } else {
          // Introduce a fresh type variable to represent one more level of the row.
          val restRow2 = Type.freshVar(Kind.Schema, tvar.loc)
          val type2 = Type.mkSchemaExtend(label1, fieldType1, restRow2, tvar.loc)
          val subst = Substitution.singleton(tv, type2)
          Ok((subst, restRow2))
        }

      case (Type.Cst(TypeConstructor.SchemaEmpty, _), Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label1), _), fieldType1, _), _, _)) =>
        // Case 3: The `label` does not exist in the record.
        Err(UnificationError.UndefinedPredicate(label1, fieldType1, rewrittenRow))

      case _ =>
        // Case 4: The type is not a row.
        Err(UnificationError.NonSchemaType(rewrittenRow))
    }

    visit(rewrittenRow)
  }

  /**
    * Lifts the given type `tpe` into the inference monad.
    */
  def liftM(tpe: Type): InferMonad[Type] = InferMonad(s => Ok((s, s(tpe))))

  /**
    * Lifts the given type `tpe` and effect `eff` into the inference monad.
    */
  def liftM(tpe: Type, eff: Type): InferMonad[(Type, Type)] = InferMonad(s => Ok((s, (s(tpe), s(eff)))))

  /**
    * Lifts the given type `tpe` and effect `eff` into the inference monad.
    */
  def liftM(constraints: List[Ast.TypeConstraint], tpe: Type, eff: Type): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
    InferMonad(s => Ok((s, (constraints.map(s.apply), s(tpe), s(eff)))))

  /**
    * Unifies the two given types `tpe1` and `tpe2` lifting their unified types and
    * associated substitution into the type inference monad.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution) => {
      val type1 = s(tpe1)
      val type2 = s(tpe2)
      unifyTypes(type1, type2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(tpe1))

        case Result.Err(UnificationError.MismatchedTypes(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.MismatchedBools(baseType1, baseType2)) =>
          Err(TypeError.MismatchedBools(baseType1, baseType2, Some(type1), Some(type2), loc))

        case Result.Err(UnificationError.MismatchedArity(baseType1, baseType2)) =>
          Err(TypeError.MismatchedArity(tpe1, tpe2, loc))

        case Result.Err(UnificationError.RigidVar(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.OccursCheck(baseType1, baseType2)) =>
          Err(TypeError.OccursCheckError(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.UndefinedField(fieldName, fieldType, recordType)) =>
          Err(TypeError.UndefinedField(fieldName, fieldType, recordType, loc))

        case Result.Err(UnificationError.NonRecordType(tpe)) =>
          Err(TypeError.NonRecordType(tpe, loc))

        case Result.Err(UnificationError.UndefinedPredicate(predSym, predType, schemaType)) =>
          Err(TypeError.UndefinedPredicate(predSym, predType, schemaType, loc))

        case Result.Err(UnificationError.NonSchemaType(tpe)) =>
          Err(TypeError.NonSchemaType(tpe, loc))

        case Result.Err(UnificationError.MismatchedKinds(kind1, kind2)) =>
          Err(TypeError.MismatchedKinds(type1, type2, kind1, kind2, loc))

        case Result.Err(err: UnificationError.NoMatchingInstance) =>
          throw InternalCompilerException(s"Unexpected unification error: $err")

        case Result.Err(err: UnificationError.MultipleMatchingInstances) =>
          throw InternalCompilerException(s"Unexpected unification error: $err")
      }
    }
    )
  }

  /**
    * Unifies the three given types `tpe1`, `tpe2`, and `tpe3`.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypeM(List(tpe1, tpe2, tpe3), loc)

  /**
    * Unifies the four given types `tpe1`, `tpe2`, `tpe3` and `tpe4`.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypeM(List(tpe1, tpe2, tpe3, tpe4), loc)

  /**
    * Unifies all the types in the given non-empty list `ts`.
    */
  def unifyTypeM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyTypeM(tpe, y, loc), ys)
      }
    }

    visit(liftM(ts.head), ts.tail)
  }

  /**
    * Unifies all the types in the given (possibly empty) list `ts`.
    */
  def unifyTypeAllowEmptyM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    if (ts.isEmpty)
      liftM(Type.freshVar(Kind.Star, loc))
    else
      unifyTypeM(ts, loc)
  }

  /**
    * Unifies the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unifyBoolM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    // Return if Boolean unification is disabled.
    if (flix.options.xnoboolunification)
      return liftM(Type.True)

    InferMonad((s: Substitution) => {
      val bf1 = s(tpe1)
      val bf2 = s(tpe2)
      BoolUnification.unify(bf1, bf2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(tpe1))

        case Result.Err(e) => e match {
          case UnificationError.MismatchedBools(baseType1, baseType2) =>
            Err(TypeError.MismatchedBools(baseType1, baseType2, None, None, loc))

          case _ => throw InternalCompilerException(s"Unexpected error: '$e'.")
        }
      }
    }
    )
  }

  /**
    * Unifies the three given Boolean formulas `tpe1`, `tpe2`, and `tpe3`.
    */
  def unifyBoolM(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] =
    unifyBoolM(List(tpe1, tpe2, tpe3), loc)

  /**
    * Unifies all the Boolean formulas in the given non-empty list `fs`.
    */
  def unifyBoolM(fs: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyBoolM(tpe, y, loc), ys)
      }
    }

    visit(liftM(fs.head), fs.tail)
  }

  /**
    * Removes the given type variable from the substitution.
    *
    * NB: Use with EXTREME CAUTION.
    */
  def unbindVar(tvar: Type.KindedVar): InferMonad[Unit] =
    InferMonad(s => {
      Ok((s.unbind(tvar), ()))
    })

  /**
    * Purifies the given effect `eff` in the type inference monad.
    */
  def purifyEffM(tvar: Type.KindedVar, eff: Type): InferMonad[Type] =
    InferMonad(s => {
      val purifiedEff = purify(tvar, s(eff))
      Ok((s, purifiedEff))
    })

  /**
    * Returns the given Boolean formula `tpe` with the (possibly rigid) type variable `tvar` replaced by `True`.
    */
  private def purify(tvar: Type.KindedVar, tpe: Type): Type = tpe.typeConstructor match {
    case None => tpe match {
      case t: Type.Var =>
        if (tvar.id == t.asKinded.id) Type.True else tpe
      case _ => throw InternalCompilerException(s"Unexpected type constructor: '$tpe'.")
    }

    case Some(tc) => tc match {
      case TypeConstructor.True => Type.True

      case TypeConstructor.False => Type.False

      case TypeConstructor.Not =>
        val List(t) = tpe.typeArguments
        BoolUnification.mkNot(purify(tvar, t))

      case TypeConstructor.And =>
        val List(t1, t2) = tpe.typeArguments
        BoolUnification.mkAnd(purify(tvar, t1), purify(tvar, t2))

      case TypeConstructor.Or =>
        val List(t1, t2) = tpe.typeArguments
        BoolUnification.mkOr(purify(tvar, t1), purify(tvar, t2))

      case _ => throw InternalCompilerException(s"Unexpected non-Boolean type constructor: '$tc'.")
    }
  }

  /**
    * Returns true iff `tpe1` unifies with `tpe2`.
    */
  def unifiesWith(tpe1: Type, tpe2: Type)(implicit flix: Flix): Boolean = {
    Unification.unifyTypes(tpe1, tpe2) match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }
  }

}
