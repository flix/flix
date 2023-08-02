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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, LevelEnv, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.collection.{Bimap, ListMap, MultiMap}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object EqualityEnvironment {

  /**
    * Checks that the given `econstrs` entail the given `econstr`.
    */
  def entail(econstrs: List[Ast.EqualityConstraint], econstr: Ast.BroadEqualityConstraint, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, UnificationError] = {
    // create assoc-type substitution using econstrs
    val subst = toSubst(econstrs)

    // extract the types
    val Ast.BroadEqualityConstraint(tpe1, tpe2) = econstr

    // apply the substitution to them
    val newTpe1 = subst(tpe1)
    val newTpe2 = subst(tpe2)

    // check that econstr becomes tautological (according to global instance map)
    for {
      res1 <- reduceType(newTpe1, eqEnv)
      res2 <- reduceType(newTpe2, eqEnv)
      // after reduction, the only associated types that remain are over rigid variables T[α]
      // (non-rigid variables would cause an instance error)
      res <- unifyHard(res1, res2, renv, LevelEnv.Unleveled) match {
        case Result.Ok(subst) => Result.Ok(subst): Result[Substitution, UnificationError]
        case Result.Err(_) => Result.Err(UnificationError.UnsupportedEquality(res1, res2)): Result[Substitution, UnificationError]
      }
      // TODO ASSOC-TYPES weird typing hack
    } yield res
  }.toValidation

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

  // MATT docs
  // MATT inline docs
  private def unifyHard(tpe1: Type, tpe2: Type, renv0: RigidityEnv, lenv: LevelEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    val allAssocs = assocs(tpe1) ++ assocs(tpe2)

    val env = allAssocs.foldLeft(Bimap.empty[RigidAssoc, Symbol.KindedTypeVarSym]) {
      (acc, assoc) => acc + (assoc -> Symbol.freshKindedTypeVarSym(Ast.VarText.Absent, Kind.Wild, isRegion = false, SourceLocation.Unknown))
    }

    val subst = env.m1.foldLeft(AssocTypeSubstitution.empty) {
      case (acc, (RigidAssoc(assocSym, varSym, _), freshVar)) => acc ++ AssocTypeSubstitution.singleton(assocSym, varSym, Type.Var(freshVar, SourceLocation.Unknown))
    }

    val unsubst = env.m2.foldLeft(Substitution.empty) {
      case (acc, (freshVar, RigidAssoc(assocSym, varSym, kind))) => acc ++ Substitution.singleton(freshVar, Type.AssocType(Ast.AssocTypeConstructor(assocSym, SourceLocation.Unknown), Type.Var(varSym, SourceLocation.Unknown), kind, SourceLocation.Unknown))
    }

    val renv = env.m1.values.foldLeft(renv0)(_.markRigid(_))

    val res = Unification.unifyTypes(subst(tpe1), subst(tpe2), renv, lenv)

    res.map {
      case (s, Nil) => unsubst @@ s
      case (_, _ :: _) => throw InternalCompilerException("unexpected associated type", SourceLocation.Unknown)
    }
  }

  private case class RigidAssoc(assocSym: Symbol.AssocTypeSym, varSym: Symbol.KindedTypeVarSym, kind: Kind)

  private def assocs(tpe: Type): Set[RigidAssoc] = tpe match {
    case Type.Var(_, _) => Set.empty
    case Type.Cst(_, _) => Set.empty
    case Type.Apply(tpe1, tpe2, _) => assocs(tpe1) ++ assocs(tpe2)
    case Type.Alias(_, _, tpe, _) => assocs(tpe)
    case Type.AssocType(Ast.AssocTypeConstructor(assocSym, _), Type.Var(varSym, _), kind, _) => Set(RigidAssoc(assocSym, varSym, kind))
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException("unexpected non-variable associated type", SourceLocation.Unknown)
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
  def reduceAssocTypeStep(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
    val renv = arg.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
    val insts = eqEnv(cst.sym)
    insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
      inst =>
        Unification.unifyTypes(arg, inst.arg, renv, LevelEnv.Unleveled).toOption.map { // TODO level env?
          case (subst, econstrs) => subst(inst.ret) // TODO ASSOC-TYPES consider econstrs
        }
    }.nextOption() match {
      case None => Result.Err(UnificationError.IrreducibleAssocType(cst.sym, arg))
      case Some(t) => Result.Ok(t)
    }
  }

  /**
    * Fully reduces the given associated type.
    */
  def reduceAssocType(cst: Ast.AssocTypeConstructor, arg: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
    for {
      tpe <- reduceAssocTypeStep(cst, arg, eqEnv)
      res <- reduceType(tpe, eqEnv)
    } yield res
  }

  /**
    * Reduces associated types in the equality environment.
    */
  def reduceType(t0: Type, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[Type, UnificationError] = {
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
          // MATT ugly hack for testing
          res <- reduceAssocTypeStep(cst, arg, eqEnv) match {
            case Result.Ok(res0) => visit(res0): Result[Type, UnificationError]
            case Result.Err(_) => Result.Ok(Type.AssocType(cst, arg, kind, loc)): Result[Type, UnificationError]
          }
//          res0 <- reduceAssocTypeStep(cst, arg, kind, eqEnv)
//          res <- visit(res0)
        } yield res
    }

    visit(t0)
  }

}
