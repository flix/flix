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
import ca.uwaterloo.flix.language.ast.{Rigidity, SourceLocation, Type}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

object Unification {

  /**
    * Unifies the given variable `x` with the given non-variable type `tpe`.
    */
  private def unifyVar(x: Type.Var, tpe: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = {
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

    //if (x.kind != tpe.kind) {
    //  return Result.Err(TypeError.KindError())
    //}

    // We can substitute `x` for `tpe`. Update the textual name of `tpe`.
    if (x.getText.nonEmpty && tpe.isInstanceOf[Type.Var]) {
      // TODO: Get rid of this insanity.
      tpe.asInstanceOf[Type.Var].setText(x.getText.get)
    }
    Result.Ok(Substitution.singleton(x, tpe))
  }

  /**
    * Unifies the two given types `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def unifyTypes(tpe1: Type, tpe2: Type)(implicit flix: Flix): Result[Substitution, UnificationError] = (tpe1, tpe2) match {
    case (x: Type.Var, y: Type.Var) =>
      // Case 1: Check if the type variables are syntactically the same.
      if (x.id == y.id && x.kind == y.kind)
        return Result.Ok(Substitution.empty)
      // Case 2: The left type variable is flexible.
      if (x.rigidity == Rigidity.Flexible)
        return Result.Ok(Substitution.singleton(x, y))
      // Case 3: The right type variable is flexible.
      if (y.rigidity == Rigidity.Flexible)
        return Result.Ok(Substitution.singleton(y, x))
      // Case 4: Both type variables are rigid.
      Result.Err(UnificationError.RigidVar(x, y))

    case (x: Type.Var, _) => unifyVar(x, tpe2)

    case (_, x: Type.Var) => unifyVar(x, tpe1)

    case (Type.Cst(c1), Type.Cst(c2)) if c1 == c2 => Result.Ok(Substitution.empty)

    case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
      unifyTypes(t11, t21) match {
        case Result.Ok(subst1) => unifyTypes(subst1(t12), subst1(t22)) match {
          case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }

    case (Type.Arrow(l1, eff1), Type.Arrow(l2, eff2)) if l1 == l2 => BoolUnification.unifyEffects(eff1, eff2)

    case (Type.RecordEmpty, Type.RecordEmpty) => Result.Ok(Substitution.empty)

    case (Type.SchemaEmpty, Type.SchemaEmpty) => Result.Ok(Substitution.empty)

    case (Type.RecordExtend(label1, fieldType1, restRow1), row2) =>
      // Attempt to write the row to match.
      rewriteRow(row2, label1, fieldType1, row2) flatMap {
        case (subst1, restRow2) =>
          // TODO: Missing the safety/occurs check.
          unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
            case subst2 => Result.Ok(subst2 @@ subst1)
          }
      }

    case (Type.SchemaExtend(sym, tpe, restRow1), row2) =>
      // Attempt to write the row to match.
      rewriteSchemaRow(row2, sym, tpe, row2) flatMap {
        case (subst1, restRow2) =>
          // TODO: Missing the safety/occurs check.
          unifyTypes(subst1(restRow1), subst1(restRow2)) flatMap {
            case subst2 => Result.Ok(subst2 @@ subst1)
          }
      }

    case (Type.Zero, Type.Zero) => Result.Ok(Substitution.empty) // 0 == 0

    case (Type.Succ(0, Type.Zero), Type.Zero) => Result.Ok(Substitution.empty)

    case (Type.Zero, Type.Succ(0, Type.Zero)) => Result.Ok(Substitution.empty)

    case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 == n2 => unifyTypes(t1, t2) //(42, t1) == (42, t2)

    case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 > n2 => unifyTypes(Type.Succ(n1 - n2, t1), t2) // (42, x) == (21 y) --> (42-21, x) = y

    case (Type.Succ(n1, t1), Type.Succ(n2, t2)) if n1 < n2 => unifyTypes(Type.Succ(n2 - n1, t2), t1) // (21, x) == (42, y) --> (42-21, y) = x

    case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
  }

  /**
    * Attempts to rewrite the given row type `row2` into a row that has the given label `label1` in front.
    */
  private def rewriteRow(row2: Type, label1: String, fieldType1: Type, originalType: Type)(implicit flix: Flix): Result[(Substitution, Type), UnificationError] = row2 match {
    case Type.RecordExtend(label2, fieldType2, restRow2) =>
      // Case 1: The row is of the form %{ label2: fieldType2 | restRow2 }
      if (label1 == label2) {
        // Case 1.1: The labels match, their types must match.
        for {
          subst <- unifyTypes(fieldType1, fieldType2)
        } yield (subst, restRow2)
      } else {
        // Case 1.2: The labels do not match, attempt to match with a label further down.
        rewriteRow(restRow2, label1, fieldType1, originalType) map {
          case (subst, rewrittenRow) => (subst, Type.RecordExtend(label2, fieldType2, rewrittenRow))
        }
      }
    case tvar: Type.Var =>
      // Case 2: The row is a type variable.
      // Introduce a fresh type variable to represent one more level of the row.
      val restRow2 = Type.freshTypeVar()
      val type2 = Type.RecordExtend(label1, fieldType1, restRow2)
      val subst = Substitution.singleton(tvar, type2)
      Ok((subst, restRow2))

    case Type.RecordEmpty =>
      // Case 3: The `label` does not exist in the record.
      Err(UnificationError.UndefinedLabel(label1, fieldType1, originalType))

    case _ =>
      // Case 4: The type is not a row.
      Err(UnificationError.NonRecordType(row2))
  }

  /**
    * Attempts to rewrite the given row type `row2` into a row that has the given label `label1` in front.
    */
  // TODO: This is a copy of the above function. It would be nice if it could be the same function, but the shape of labels is different.
  private def rewriteSchemaRow(row2: Type, label1: String, fieldType1: Type, originalType: Type)(implicit flix: Flix): Result[(Substitution, Type), UnificationError] = row2 match {
    case Type.SchemaExtend(label2, fieldType2, restRow2) =>
      // Case 1: The row is of the form %{ label2: fieldType2 | restRow2 }
      if (label1 == label2) {
        // Case 1.1: The labels match, their types must match.
        for {
          subst <- unifyTypes(fieldType1, fieldType2)
        } yield (subst, restRow2)
      } else {
        // Case 1.2: The labels do not match, attempt to match with a label further down.
        rewriteSchemaRow(restRow2, label1, fieldType1, originalType) map {
          case (subst, rewrittenRow) => (subst, Type.SchemaExtend(label2, fieldType2, rewrittenRow))
        }
      }
    case tvar: Type.Var =>
      // Case 2: The row is a type variable.
      // Introduce a fresh type variable to represent one more level of the row.
      val restRow2 = Type.freshTypeVar()
      val type2 = Type.SchemaExtend(label1, fieldType1, restRow2)
      val subst = Substitution.singleton(tvar, type2)
      Ok((subst, restRow2))

    case Type.SchemaEmpty =>
      // Case 3: The `label` does not exist in the record.
      Err(UnificationError.UndefinedPredicate(label1, fieldType1, originalType))

    case _ =>
      // Case 4: The type is not a row.
      Err(UnificationError.NonSchemaType(row2))
  }

  /**
    * Returns `true` if `tpe1` is an instance of `tpe2`.
    */
  def isInstance(tpe1: Type, tpe2: Type)(implicit flix: Flix): Boolean =
    Unification.unifyTypes(tpe1, tpe2) match {
      case Ok(_) => true
      case Err(_) => false
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
    * Unifies the two given types `tpe1` and `tpe2` lifting their unified types and
    * associated substitution into the type inference monad.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution) => {
      val type1 = s(tpe1)
      val type2 = s(tpe2)
      unifyTypes(type1, type2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(tpe1))

        case Result.Err(UnificationError.MismatchedTypes(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.MismatchedEffects(baseType1, baseType2)) =>
          Err(TypeError.MismatchedEffects(baseType1, baseType2, loc))

        case Result.Err(UnificationError.MismatchedArity(baseType1, baseType2)) =>
          Err(TypeError.MismatchedArity(tpe1, tpe2, loc))

        case Result.Err(UnificationError.RigidVar(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.OccursCheck(baseType1, baseType2)) =>
          Err(TypeError.OccursCheckError(baseType1, baseType2, type1, type2, loc))

        case Result.Err(UnificationError.UndefinedLabel(fieldName, fieldType, recordType)) =>
          Err(TypeError.UndefinedField(fieldName, fieldType, recordType, loc))

        case Result.Err(UnificationError.NonRecordType(tpe)) =>
          Err(TypeError.NonRecordType(tpe, loc))

        case Result.Err(UnificationError.UndefinedPredicate(predSym, predType, schemaType)) =>
          Err(TypeError.UndefinedPredicate(predSym, predType, schemaType, loc))

        case Result.Err(UnificationError.NonSchemaType(tpe)) =>
          Err(TypeError.NonSchemaType(tpe, loc))
      }
    }
    )
  }

  /**
    * Unifies the three given types `tpe1`, `tpe2`, and `tpe3`.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypM(List(tpe1, tpe2, tpe3), loc)

  /**
    * Unifies the four given types `tpe1`, `tpe2`, `tpe3` and `tpe4`.
    */
  def unifyTypM(tpe1: Type, tpe2: Type, tpe3: Type, tpe4: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyTypM(List(tpe1, tpe2, tpe3, tpe4), loc)

  /**
    * Unifies all the types in the given non-empty list `ts`.
    */
  def unifyTypM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyTypM(tpe, y, loc), ys)
      }
    }

    visit(liftM(ts.head), ts.tail)
  }

  /**
    * Unifies all the types in the given (possibly empty) list `ts`.
    */
  def unifyTypAllowEmptyM(ts: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    if (ts.isEmpty)
      liftM(Type.freshTypeVar())
    else
      unifyTypM(ts, loc)
  }

  /**
    * Unifies the two given effects `eff1` and `eff2`.
    */
  def unifyEffM(eff1: Type, eff2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    // Determine if effect checking is enabled.
    if (flix.options.xnoeffects)
      return liftM(Type.Pure)

    InferMonad((s: Substitution) => {
      val effect1 = s(eff1)
      val effect2 = s(eff2)
      BoolUnification.unifyEffects(effect1, effect2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Ok(subst, subst(eff1))

        case Result.Err(e) => e match {
          case UnificationError.MismatchedEffects(baseType1, baseType2) =>
            Err(TypeError.MismatchedEffects(baseType1, baseType2, loc))

          case _ => throw InternalCompilerException(s"Unexpected error: '$e'.")
        }
      }
    }
    )
  }

  /**
    * Unifies the three given effects `eff1`, `eff2`, and `eff3`.
    */
  def unifyEffM(eff1: Type, eff2: Type, eff3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = unifyEffM(List(eff1, eff2, eff3), loc)

  /**
    * Unifies all the effects in the given non-empty list `fs`.
    */
  def unifyEffM(fs: List[Type], loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    def visit(x0: InferMonad[Type], xs: List[Type]): InferMonad[Type] = xs match {
      case Nil => x0
      case y :: ys => x0 flatMap {
        case tpe => visit(unifyEffM(tpe, y, loc), ys)
      }
    }

    visit(liftM(fs.head), fs.tail)
  }

}
