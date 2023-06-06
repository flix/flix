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
import ca.uwaterloo.flix.language.phase.Regions
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object Unification {

  /**
    * Unify the two type variables `x` and `y`.
    */
  private def unifyVars(x: Type.Var, y: Type.Var, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    // Case 0: types are identical
    if (x.sym == y.sym) {
      Result.Ok(Substitution.empty, Nil)
    } else {
      (renv.get(x.sym), renv.get(y.sym)) match {
        // Case 1: x is flexible
        case (Rigidity.Flexible, _) => Result.Ok(Substitution.singleton(x.sym, y), Nil)
        // Case 2: y is flexible
        case (_, Rigidity.Flexible) => Result.Ok(Substitution.singleton(y.sym, x), Nil)
        // Case 3: both variables are rigid
        case (Rigidity.Rigid, Rigidity.Rigid) => Result.Err(UnificationError.RigidVar(x, y))
      }
    }
  }

  /**
    * Unifies the given variable `x` with the given non-variable type `tpe`.
    */
  def unifyVar(x: Type.Var, tpe: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = tpe match {

    // ensure the kinds are compatible
    case _ if !KindUnification.unifiesWith(x.kind, tpe.kind) => Result.Err(UnificationError.MismatchedTypes(x, tpe))

    case y: Type.Var => unifyVars(x, y, renv)

    // No rigidity/occurs check for associated types
    // TODO ASSOC-TYPES probably the same situation for type aliases
    case assoc: Type.AssocType =>
      // don't do the substitution if the var is in the assoc type
      if (assoc.typeVars contains x) {
        Result.Ok((Substitution.empty, List(Ast.BroadEqualityConstraint(x, assoc))))
      } else {
        Result.Ok((Substitution.singleton(x.sym, assoc), Nil))
      }

    case _ =>

      // Check if `x` is rigid.
      if (renv.isRigid(x.sym)) {
        return Result.Err(UnificationError.RigidVar(x, tpe))
      }

      // Check if `x` occurs within `tpe`.
      if (tpe.typeVars contains x) {
        return Result.Err(UnificationError.OccursCheck(x, tpe))
      }

      Result.Ok(Substitution.singleton(x.sym, tpe), Nil)
  }

  /**
    * Unifies the two given types `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def unifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = (tpe1.kind, tpe2.kind) match {

    //
    // Bools
    //
    case (Kind.Eff, Kind.Eff) =>
      // don't try to unify effects if the `no-bool-effects` flag is on
      if (flix.options.xnobooleffects) {
        Ok(Substitution.empty, Nil)
      } else {
        EffUnification.unify(tpe1, tpe2, renv)
      }

    case (Kind.Bool, Kind.Bool) => BoolUnification.unify(tpe1, tpe2, renv)

    case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
      val cases = sym1.universe
      CaseSetUnification.unify(tpe1, tpe2, renv, cases, sym1).map((_, Nil)) // TODO ASSOC-TYPES support in sets

    //
    // Record Rows
    //
    case (Kind.RecordRow, Kind.RecordRow) => RecordUnification.unifyRows(tpe1, tpe2, renv)

    //
    // Schema Rows
    //
    case (Kind.SchemaRow, Kind.SchemaRow) => SchemaUnification.unifyRows(tpe1, tpe2, renv).map((_, Nil)) // TODO ASSOC-TYPES support in rows

    //
    // Other: Star or Arrow
    //
    case _ => unifyStarOrArrowTypes(tpe1, tpe2, renv)
  }

  /**
    * Unifies the types `tpe1` and `tpe2`.
    * The types must each have a Star or Arrow kind.
    */
  private def unifyStarOrArrowTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = (tpe1, tpe2) match {

    case (x: Type.Var, _) => unifyVar(x, tpe2, renv)

    case (_, x: Type.Var) => unifyVar(x, tpe1, renv)

    case (Type.Cst(c1, _), Type.Cst(c2, _)) if c1 == c2 => Result.Ok(Substitution.empty, Nil)

    case (Type.Alias(_, _, tpe, _), _) => unifyTypes(tpe, tpe2, renv)

    case (_, Type.Alias(_, _, tpe, _)) => unifyTypes(tpe1, tpe, renv)

    case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) =>
      unifyTypes(t11, t21, renv) match {
        case Result.Ok((subst1, econstrs1)) => unifyTypes(subst1(t12), subst1(t22), renv) match {
          case Result.Ok((subst2, econstrs2)) => Result.Ok(subst2 @@ subst1, econstrs1 ++ econstrs2) // TODO ASSOC-TYPES do we need to subst on econstrs?
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }

    case (Type.AssocType(cst1, args1, _, _), Type.AssocType(cst2, args2, _, _)) if cst1.sym == cst2.sym && args1 == args2 => Result.Ok(Substitution.empty, Nil)

    case (_: Type.AssocType, _) => Result.Ok(Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2)))

    case (_, _: Type.AssocType) => Result.Ok(Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2)))

    case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
  }

  /**
    * Lifts the given type `tpe` into the inference monad.
    */
  def liftM(tpe: Type): InferMonad[Type] = InferMonad { case (s, econstrs, renv) => Ok((s, econstrs, renv, s(tpe))) }

  /**
    * Lifts the given type constraints, type, purity, and effect into the inference monad.
    */
  def liftM(tconstrs: List[Ast.TypeConstraint], tpe: Type, eff: Type): InferMonad[(List[Ast.TypeConstraint], Type, Type)] =
    InferMonad { case (s, econstrs, renv) => Ok((s, econstrs, renv, (tconstrs.map(s.apply), s(tpe), s(eff)))) }

  /**
    * Unifies the two given types `tpe1` and `tpe2` lifting their unified types and
    * associated substitution into the type inference monad.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution, econstrs: List[Ast.BroadEqualityConstraint], renv: RigidityEnv) => {
      val type1 = s(tpe1)
      val type2 = s(tpe2)
      unifyTypes(type1, type2, renv) match {
        case Result.Ok((s1, econstrs1)) =>
          val subst = s1 @@ s
          val e = econstrs1 ++ econstrs
          Ok((subst, e, renv, subst(tpe1))) // TODO ASSOC-TYPES need to apply subst?

        case Result.Err(UnificationError.MismatchedTypes(baseType1, baseType2)) =>
          (baseType1.typeConstructor, baseType2.typeConstructor) match {
            case (Some(TypeConstructor.Arrow(_)), _) => Err(getUnderOrOverAppliedError(baseType1, baseType2, type1, type2, renv, loc))
            case (_, Some(TypeConstructor.Arrow(_))) => Err(getUnderOrOverAppliedError(baseType2, baseType1, type2, type1, renv, loc))
            case _ => Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, renv, loc))
          }

        case Result.Err(UnificationError.MismatchedBools(baseType1, baseType2)) =>
          (tpe1.typeConstructor, tpe2.typeConstructor) match {
            case (Some(TypeConstructor.Arrow(_)), _) => Err(TypeError.MismatchedArrowBools(baseType1, baseType2, type1, type2, renv, loc))
            case (_, Some(TypeConstructor.Arrow(_))) => Err(TypeError.MismatchedArrowBools(baseType1, baseType2, type1, type2, renv, loc))
            case _ => Err(TypeError.MismatchedBools(baseType1, baseType2, type1, type2, renv, loc))
          }

        case Result.Err(UnificationError.MismatchedArity(_, _)) =>
          Err(TypeError.MismatchedArity(tpe1, tpe2, renv, loc))

        case Result.Err(UnificationError.RigidVar(baseType1, baseType2)) =>
          Err(TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, renv, loc))

        case Result.Err(UnificationError.OccursCheck(baseType1, baseType2)) =>
          Err(TypeError.OccursCheckError(baseType1, baseType2, type1, type2, renv, loc))

        case Result.Err(UnificationError.UndefinedField(fieldName, fieldType, recordType)) =>
          Err(TypeError.UndefinedField(fieldName, fieldType, recordType, renv, loc))

        case Result.Err(UnificationError.NonRecordType(tpe)) =>
          Err(TypeError.NonRecordType(tpe, renv, loc))

        case Result.Err(UnificationError.UndefinedPredicate(predSym, predType, schemaType)) =>
          Err(TypeError.UndefinedPredicate(predSym, predType, schemaType, renv, loc))

        case Result.Err(UnificationError.NonSchemaType(tpe)) =>
          Err(TypeError.NonSchemaType(tpe, renv, loc))

        case Result.Err(UnificationError.HackError(message)) =>
          Err(TypeError.HackError(message, loc))

        case Result.Err(err: UnificationError.NoMatchingInstance) =>
          throw InternalCompilerException(s"Unexpected unification error: $err", loc)

        case Result.Err(err: UnificationError.MultipleMatchingInstances) =>
          throw InternalCompilerException(s"Unexpected unification error: $err", loc)

        case Result.Err(err: UnificationError.IrreducibleAssocType) =>
          throw InternalCompilerException(s"Unexpected unification error: $err", loc)

        case Result.Err(err: UnificationError.UnsupportedEquality) =>
          throw InternalCompilerException(s"Unexpected unification error: $err", loc)
      }
    })
  }

  /**
    * Unifies the `expected` type with the `actual` type.
    */
  def expectTypeM(expected: Type, actual: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    // Note: The handler should *NOT* use `expected` nor `actual` since they have not had their variables substituted.
    def handler(e: TypeError): TypeError = e match {
      case TypeError.MismatchedTypes(baseType1, baseType2, fullType1, fullType2, renv, _) =>
        (baseType1.typeConstructor, baseType2.typeConstructor) match {
          case (Some(TypeConstructor.Native(left)), Some(TypeConstructor.Native(right))) if left.isAssignableFrom(right) =>
            TypeError.PossibleCheckedTypeCast(expected, actual, renv, loc)
          case _ =>
            TypeError.UnexpectedType(baseType1, baseType2, renv, baseType2.loc)
        }
      case e => e
    }

    unifyTypeM(expected, actual, loc).transformError(handler)
  }

  /**
    * Unifies the `expected` type with the `actual` type (and unifies `bind` with the result).
    */
  def expectTypeM(expected: Type, actual: Type, bind: Type.Var, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    for {
      r <- expectTypeM(expected, actual, loc)
      _ <- unifyTypeM(bind, r, loc)
    } yield r
  }

  /**
    * Unifies the `expectedTypes` types with the `actualTypes`.
    */
  def expectTypeArguments(sym: Symbol, expectedTypes: List[Type], actualTypes: List[Type], actualLocs: List[SourceLocation], loc: SourceLocation)(implicit flix: Flix): InferMonad[Unit] = {
    // Note: The handler should *NOT* use `expectedTypes` nor `actualTypes` since they have not had their variables substituted.
    def handler(i: Int)(e: TypeError): TypeError = e match {
      case TypeError.MismatchedBools(_, _, fullType1, fullType2, renv, loc) =>
        TypeError.UnexpectedArgument(sym, i, fullType1, fullType2, renv, loc)

      case TypeError.MismatchedArrowBools(_, _, fullType1, fullType2, renv, loc) =>
        TypeError.UnexpectedArgument(sym, i, fullType1, fullType2, renv, loc)

      case TypeError.MismatchedTypes(_, _, fullType1, fullType2, renv, loc) =>
        TypeError.UnexpectedArgument(sym, i, fullType1, fullType2, renv, loc)
      case e => e
    }

    def visit(i: Int, expected: List[Type], actual: List[Type], locs: List[SourceLocation]): InferMonad[Unit] =
      (expected, actual, locs) match {
        case (Nil, Nil, Nil) => InferMonad.point(())
        case (x :: xs, y :: ys, l :: ls) =>
          for {
            _ <- unifyTypeM(x, y, l).transformError(handler(i))
          } yield visit(i + 1, xs, ys, ls)
        case (missingArg :: _, Nil, _) => InferMonad.errPoint(TypeError.UnderApplied(missingArg, loc))
        case (Nil, excessArg :: _l, _) => InferMonad.errPoint(TypeError.OverApplied(excessArg, loc))
        case _ => throw InternalCompilerException("Mismatched lists.", loc)
      }

    visit(1, expectedTypes, actualTypes, actualLocs)
  }

  /**
    * Returns a [[TypeError.OverApplied]] or [[TypeError.UnderApplied]] type error, if applicable.
    */
  private def getUnderOrOverAppliedError(arrowType: Type, argType: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): TypeError = {
    val default = TypeError.MismatchedTypes(arrowType, argType, fullType1, fullType2, renv, loc)

    arrowType match {
      case Type.Apply(_, resultType, _) =>
        if (Unification.unifiesWith(resultType, argType, renv, ListMap.empty)) { // TODO ASSOC-TYPES empty OK?
          arrowType.typeArguments.lift(1) match {
            case None => default
            case Some(excessArgument) => TypeError.OverApplied(excessArgument, loc)
          }
        } else {
          arrowType.typeArguments.lift(1) match {
            case None => default
            case Some(missingArgument) => TypeError.UnderApplied(missingArgument, loc)
          }
        }
      case _ => default
    }
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
  def unifyTypeAllowEmptyM(ts: List[Type], kind: Kind, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    if (ts.isEmpty)
      liftM(Type.freshVar(kind, loc))
    else
      unifyTypeM(ts, loc)
  }

  /**
    * Unifies the two given effect formulas `tpe1` and `tpe2`.
    */
  def unifyEffM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution, econstrs: List[Ast.BroadEqualityConstraint], renv: RigidityEnv) => {
      val bf1 = s(tpe1)
      val bf2 = s(tpe2)
      EffUnification.unify(bf1, bf2, renv) match {
        case Result.Ok((s1, econstrs1)) =>
          val subst = s1 @@ s
          val e = econstrs1 ++ econstrs
          Ok((subst, e, renv, subst(tpe1))) // TODO ASSOC-TYPES need to apply subst?

        case Result.Err(e) => e match {
          case UnificationError.MismatchedBools(baseType1, baseType2) =>
            Err(TypeError.MismatchedBools(baseType1, baseType2, tpe1, tpe2, renv, loc))

          case _ => throw InternalCompilerException(s"Unexpected error: '$e'.", loc)
        }
      }
    }
    )
  }

  /**
    * Unifies the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unifyBoolM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] = {
    InferMonad((s: Substitution, econstrs: List[Ast.BroadEqualityConstraint], renv: RigidityEnv) => {
      val bf1 = s(tpe1)
      val bf2 = s(tpe2)
      BoolUnification.unify(bf1, bf2, renv) match {
        case Result.Ok((s1, econstrs1)) =>
          val subst = s1 @@ s
          val e = econstrs1 ++ econstrs
          Ok((subst, e, renv, subst(tpe1))) // TODO ASSOC-TYPES need to apply subst?

        case Result.Err(e) => e match {
          case UnificationError.MismatchedBools(baseType1, baseType2) =>
            Err(TypeError.MismatchedBools(baseType1, baseType2, tpe1, tpe2, renv, loc))

          case _ => throw InternalCompilerException(s"Unexpected error: '$e'.", loc)
        }
      }
    }
    )
  }

  /**
    * Unifies the three given effect formulas `tpe1`, `tpe2`, and `tpe3`.
    */
  def unifyEffM(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit flix: Flix): InferMonad[Type] =
    unifyEffM(List(tpe1, tpe2, tpe3), loc)

  /**
    * Unifies all the effect formulas in the given non-empty list `fs`.
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

  /**
    * Removes the given type variable `tvar` from the substitution.
    *
    * NB: Use with EXTREME CAUTION.
    */
  def unbindVar(tvar: Type.Var): InferMonad[Unit] =
    InferMonad {
      case (s, econstrs, renv) => Ok((s.unbind(tvar.sym), econstrs, renv, ()))
    }

  /**
    * Purifies the given effect `eff` in the type inference monad.
    */
  def purifyEffM(tvar: Type.Var, eff: Type): InferMonad[Type] =
    InferMonad { case (s, econstrs, renv) =>
      val purifiedEff = purify(tvar, s(eff))
      Ok((s, econstrs, renv, purifiedEff))
    }

  /**
    * Returns the given effect formula `tpe` with the (possibly rigid) type variable `tvar` replaced by `Pure`.
    */
  private def purify(tvar: Type.Var, tpe: Type): Type = tpe.typeConstructor match {
    case None => tpe match {
      case t: Type.Var =>
        if (tvar.sym == t.sym) Type.Pure else tpe
      case _ => throw InternalCompilerException(s"Unexpected type constructor: '$tpe'.", tpe.loc)
    }

    case Some(tc) => tc match {
      case TypeConstructor.Pure => Type.Pure

      case TypeConstructor.EffUniv => Type.EffUniv

      case TypeConstructor.Complement =>
        val List(t) = tpe.typeArguments
        Type.mkComplement(purify(tvar, t), tpe.loc)

      case TypeConstructor.Union =>
        val List(t1, t2) = tpe.typeArguments
        Type.mkUnion(purify(tvar, t1), purify(tvar, t2), tpe.loc)

      case TypeConstructor.Intersection =>
        val List(t1, t2) = tpe.typeArguments
        Type.mkIntersection(purify(tvar, t1), purify(tvar, t2), tpe.loc)

      case _ => throw InternalCompilerException(s"Unexpected non-effect type constructor: '$tc'.", tpe.loc)
    }
  }

  /**
    * Ensures that the region variable `rvar` does not escape in the type `tpe` nor from the context.
    */
  def noEscapeM(rvar: Type.Var, tpe: Type)(implicit flix: Flix): InferMonad[Unit] =
    InferMonad { case (s, econstrs, renv) =>
      // Apply the current substitution to `tpe`.
      val t = TypeMinimization.minimizeType(s(tpe))

      // Check whether the region variable is essential to the type.
      if (Regions.essentialTo(rvar, t)) {
        Err(TypeError.RegionVarEscapes(rvar, t, rvar.loc))
      } else
        Ok((s, econstrs, renv, ()))
    }

  /**
    * Sets the given variable as rigid in the type inference monad.
    */
  def rigidifyM(rvar: Type.Var): InferMonad[Unit] =
    InferMonad {
      case (s, econstrs, renv) => Ok((s, econstrs, renv.markRigid(rvar.sym), ()))
    }

  /**
    * Returns true iff `tpe1` unifies with `tpe2`, without introducing equality constraints.
    */
  def unifiesWith(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    Unification.unifyTypes(tpe1, tpe2, renv) match {
      case Result.Ok((_, econstrs)) =>
        // check that all econstrs hold under the environment
        econstrs.forall {
          econstr =>
            EqualityEnvironment.entail(Nil, econstr, eqEnv) match {
              case Validation.Success(_) => true
              case Validation.Failure(_) => false
              case Validation.SoftFailure(_, _) => false
            }
        }
      case Result.Err(_) => false
    }
  }

}
