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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object Unification {

  // TODO LEVELS: using top scope just to compile for now as we introduce levels
  private implicit val S: Scope = Scope.Top

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
  def unifyVar(x: Type.Var, tpe: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    tpe match {

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
  }

  /**
    * Unifies the two given types `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def unifyTypes(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = (tpe1.kind, tpe2.kind) match {

    case (Kind.Eff, Kind.Eff) => EffUnification.unify(tpe1, tpe2, renv)

    case (Kind.Bool, Kind.Bool) => BoolUnification.unify(tpe1, tpe2, renv)

    case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
      val cases = sym1.universe
      CaseSetUnification.unify(tpe1, tpe2, renv, cases, sym1).map((_, Nil)) // TODO ASSOC-TYPES support in sets

    case (Kind.RecordRow, Kind.RecordRow) => RecordUnification.unifyRows(tpe1, tpe2, renv)

    case (Kind.SchemaRow, Kind.SchemaRow) => SchemaUnification.unifyRows(tpe1, tpe2, renv).map((_, Nil)) // TODO ASSOC-TYPES support in rows

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

    case (Type.Cst(TypeConstructor.Error(_, k1), _), t2) if k1 == t2.kind => Result.Ok(Substitution.empty, Nil)

    case (t1, Type.Cst(TypeConstructor.Error(_, k2), _)) if t1.kind == k2 => Result.Ok(Substitution.empty, Nil)

    case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
  }

  /**
    * Returns a [[TypeError.OverApplied]] or [[TypeError.UnderApplied]] type error, if applicable.
    */
  def getUnderOrOverAppliedError(arrowType: Type, argType: Type, fullType1: Type, fullType2: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): TypeError = {
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
    * Returns true iff `tpe1` unifies with `tpe2`, without introducing equality constraints.
    */
  def unifiesWith(tpe1: Type, tpe2: Type, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    Unification.unifyTypes(tpe1, tpe2, renv) match {
      case Result.Ok((subst, econstrs)) =>
        // check that all econstrs hold under the environment
        econstrs.map(subst.apply).forall {
          econstr =>
            EqualityEnvironment.entail(Nil, econstr, renv, eqEnv).toHardResult match {
              case Result.Ok(_) => true
              case Result.Err(_) => false
            }
        }
      case Result.Err(_) => false
    }
  }

}
