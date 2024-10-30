/*
 * Copyright 2023 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object EqualityEnvironment {

  /**
    * Checks that the given `econstrs` entail the given `econstr`.
    */
  def entail(econstrs: List[Ast.EqualityConstraint], econstr: Ast.BroadEqualityConstraint, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[Substitution, UnificationError] = {
    // create assoc-type substitution using econstrs
    val subst = toSubst(econstrs)

    // extract the types
    val Ast.BroadEqualityConstraint(tpe1, tpe2) = econstr

    // apply the substitution to them
    val newTpe1 = subst(tpe1)
    val newTpe2 = subst(tpe2)

    // check that econstr becomes tautological (according to global instance map)
    // we specifically use the empty eqEnv for this check
    for {
      res1 <- reduceType(newTpe1, eqEnv)
      res2 <- reduceType(newTpe2, eqEnv)
      res <- Unification.fullyUnifyTypes(res1, res2, renv, ListMap.empty) match {
        case Some(subst) => Result.Ok(subst): Result[Substitution, UnificationError]
        case None => Result.Err(UnificationError.UnsupportedEquality(res1, res2)): Result[Substitution, UnificationError]
      }
      // TODO ASSOC-TYPES weird typing hack
    } yield res
  }.toValidation


  /**
    * Checks that the `givenEconstrs` entail all the given `wantedEconstrs`.
    */
  def entailAll(givenEconstrs: List[Ast.EqualityConstraint], wantedEconstrs: List[Ast.BroadEqualityConstraint], renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Validation[Substitution, UnificationError] = {
    Validation.fold(wantedEconstrs, Substitution.empty) {
      case (subst, wantedEconstr) =>
        Validation.mapN(entail(givenEconstrs, subst(wantedEconstr), renv, eqEnv)) {
          case subst1 => subst1 @@ subst
        }
    }
  }

  /**
    * Converts the given EqualityConstraint into a BroadEqualityConstraint.
    */
  def narrow(econstr: Ast.BroadEqualityConstraint): Ast.EqualityConstraint = econstr match {
    case Ast.BroadEqualityConstraint(Type.AssocType(cst, tpe1, _, _), tpe2) =>
      Ast.EqualityConstraint(cst, tpe1, tpe2, SourceLocation.Unknown)
    case _ => throw InternalCompilerException("unexpected broad equality constraint", SourceLocation.Unknown)
  }

  /**
    * Converts the given Equality
    */
  def broaden(econstr: Ast.EqualityConstraint): Ast.BroadEqualityConstraint = econstr match {
    case Ast.EqualityConstraint(cst, tpe1, tpe2, loc) =>
      Ast.BroadEqualityConstraint(Type.AssocType(cst, tpe1, Kind.Wild, loc), tpe2)
  }

  /**
    * Converts the list of equality constraints to a substitution.
    */
  private def toSubst(econstrs: List[Ast.EqualityConstraint]): AssocTypeSubstitution = {
    econstrs.foldLeft(AssocTypeSubstitution.empty) {
      case (acc, Ast.EqualityConstraint(Ast.AssocTypeConstructor(sym, _), Type.Var(tvar, _), tpe2, _)) =>
        acc ++ AssocTypeSubstitution.singleton(sym, tvar, tpe2)
      case (_, Ast.EqualityConstraint(cst, tpe1, tpe2, loc)) => throw InternalCompilerException("unexpected econstr", loc) // TODO ASSOC-TYPES
    }
  }

  /**
    * Reduces the associated type in the equality environment.
    *
    * Only performs one reduction step. The result may itself contain associated types.
    */
  def reduceAssocTypeStep(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[Type, UnificationError] = {
    val renv = arg.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
    val insts = eqEnv(cst.sym)
    insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
      inst =>
        Unification.unifyTypesIgnoreLeftoverAssocs(arg, inst.arg, renv, eqEnv).map {
          case subst => subst(inst.ret) // TODO ASSOC-TYPES consider econstrs
        }
    }.nextOption() match {
      case None => Result.Err(UnificationError.IrreducibleAssocType(cst.sym, arg))
      case Some(t) => Result.Ok(t)
    }
  }

  /**
    * Fully reduces the given associated type.
    */
  def reduceAssocType(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[Type, UnificationError] = {
    for {
      tpe <- reduceAssocTypeStep(cst, arg, eqEnv)
      res <- reduceType(tpe, eqEnv)
    } yield res
  }

  /**
    * Reduces associated types in the equality environment.
    */
  def reduceType(t0: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit scope: Scope, flix: Flix): Result[Type, UnificationError] = {
    // TODO ASSOC-TYPE require that AssocTypeDefs which themselves include assoc types are supported by tconstrs
    def visit(t: Type): Result[Type, UnificationError] = t match {
      case t: Type.Var => Result.Ok(t)
      case t: Type.Cst => Result.Ok(t)
      case Type.Apply(tpe1, tpe2, loc) =>
        visit(tpe1)
        for {
          t1 <- visit(tpe1)
          t2 <- visit(tpe2)
        } yield Type.Apply(t1, t2, loc)
      case Type.Alias(cst, args0, tpe0, loc) =>
        for {
          args <- Result.traverse(args0)(visit)
          tpe <- visit(tpe0)
        } yield Type.Alias(cst, args, tpe, loc)
      case Type.AssocType(cst, arg0, kind, loc) =>
        for {
          arg <- visit(arg0)
          res0 <- reduceAssocTypeStep(cst, arg, eqEnv)
          res <- visit(res0)
        } yield res
      case Type.JvmToType(tpe, loc) =>
        for {
          t1 <- visit(tpe)
        } yield Type.JvmToType(t1, loc)
      case Type.JvmToEff(tpe, loc) =>
        for {
          t1 <- visit(tpe)
        } yield Type.JvmToEff(t1, loc)
      case Type.UnresolvedJvmType(member0, loc) =>
        for {
          member <- traverse(member0)(visit)
        } yield Type.UnresolvedJvmType(member, loc)
    }

    visit(t0)
  }

  /**
    * Transforms `mem` by executing `f` on all the types in `this`.
    *
    * If `f` returns `Err` for any call, this function returns `Err`.
    */
  // TODO CONSTR-SOLVER-2 remove this after we migrate to the new constraint solver
  private def traverse[E](mem: Type.JvmMember)(f: Type => Result[Type, E]): Result[Type.JvmMember, E] = mem match {
    case Type.JvmMember.JvmConstructor(clazz, tpes0) =>
      for {
        tpes <- Result.traverse(tpes0)(f)
      } yield Type.JvmMember.JvmConstructor(clazz, tpes)

    case Type.JvmMember.JvmField(tpe0, name) =>
      for {
        tpe <- f(tpe0)
      } yield Type.JvmMember.JvmField(tpe, name)

    case Type.JvmMember.JvmMethod(tpe0, name, tpes0) =>
      for {
        tpe <- f(tpe0)
        tpes <- Result.traverse(tpes0)(f)
      } yield Type.JvmMember.JvmMethod(tpe, name, tpes)

    case Type.JvmMember.JvmStaticMethod(clazz, name, tpes0) =>
      for {
        tpes <- Result.traverse(tpes0)(f)
      } yield Type.JvmMember.JvmStaticMethod(clazz, name, tpes)
  }
}
