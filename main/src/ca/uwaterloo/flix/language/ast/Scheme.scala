/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatScheme}
import ca.uwaterloo.flix.language.phase.typer.ConstraintSolver.ResolutionResult
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver, TypeConstraint, TypeReduction}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnvironment, Substitution, UnificationError}
import ca.uwaterloo.flix.util.collection.{Chain, ListMap}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

object Scheme {

  /**
    * Instantiate one of the variables in the scheme, adding new quantifiers as needed.
    */
  def partiallyInstantiate(sc: Scheme, quantifier: Symbol.KindedTypeVarSym, value: Type, loc: SourceLocation)(implicit flix: Flix): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      if (!quantifiers.contains(quantifier)) {
        throw InternalCompilerException("Quantifier not in scheme.", loc)
      }
      val subst = Substitution.singleton(quantifier, value)
      val newTconstrs = tconstrs.map(subst.apply)
      val newEconstrs = econstrs.map(subst.apply)
      val newBase = subst(base)
      generalize(newTconstrs, newEconstrs, newBase, RigidityEnv.empty)
  }

  /**
    * Instantiates the given type scheme `sc` by replacing all quantified variables with fresh type variables.
    */
  def instantiate(sc: Scheme, loc: SourceLocation)(implicit flix: Flix): (List[Ast.TypeConstraint], List[Ast.BroadEqualityConstraint], Type) = {
    // Compute the base type.
    val baseType = sc.base

    //
    // Compute the fresh variables taking the instantiation mode into account.
    //
    val freshVars = sc.quantifiers.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) =>
        // Determine the rigidity of the fresh type variable.
        macc + (tvar.id -> Type.freshVar(tvar.kind, loc, tvar.isRegion, Ast.VarText.Absent))
    }

    /**
      * Replaces every variable occurrence in the given type using `freeVars`.
      *
      * Replaces all source locations by `loc`.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def visitType(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, _) =>
        // Performance: Reuse tpe0, if possible.
        freshVars.getOrElse(sym.id, tpe0)

      case Type.Cst(_, _) =>
        // Performance: Reuse tpe0.
        tpe0

      case Type.Apply(tpe1, tpe2, _) =>
        val t1 = visitType(tpe1)
        val t2 = visitType(tpe2)
        // Performance: Reuse tpe0, if possible.
        if ((t1 eq tpe1) && (t2 eq tpe2)) {
          tpe0
        } else {
          Type.Apply(t1, t2, loc)
        }

      case Type.Alias(sym, args, tpe, _) =>
        // Performance: Few aliases, not worth optimizing.
        Type.Alias(sym, args.map(visitType), visitType(tpe), loc)

      case Type.AssocType(sym, args, kind, _) =>
        // // Performance: Few associated types, not worth optimizing.
        Type.AssocType(sym, args.map(visitType), kind, loc)
    }

    val newBase = visitType(baseType)

    val newTconstrs = sc.tconstrs.map {
      case Ast.TypeConstraint(head, tpe0, loc) =>
        val tpe = tpe0.map(visitType)
        Ast.TypeConstraint(head, tpe, loc)
    }

    val newEconstrs = sc.econstrs.map {
      case Ast.BroadEqualityConstraint(tpe1, tpe2) =>
        Ast.BroadEqualityConstraint(visitType(tpe1), visitType(tpe2))
    }

    (newTconstrs, newEconstrs, newBase)
  }

  /**
    * Generalizes the given type `tpe0` with respect to the empty type environment.
    */
  def generalize(tconstrs: List[Ast.TypeConstraint], econstrs: List[Ast.BroadEqualityConstraint], tpe0: Type, renv: RigidityEnv): Scheme = {
    val tvars = tpe0.typeVars ++ tconstrs.flatMap(tconstr => tconstr.arg.typeVars) ++ econstrs.flatMap(econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars)
    val quantifiers = renv.getFlexibleVarsOf(tvars.toList)
    Scheme(quantifiers.map(_.sym), tconstrs, econstrs, tpe0)
  }

  /**
    * Returns `true` if the given schemes are equivalent.
    */
  // TODO can optimize?
  def equal(sc1: Scheme, sc2: Scheme, traitEnv: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, traitEnv, eqEnv) && lessThanEqual(sc2, sc1, traitEnv, eqEnv)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    *
    * Θₚ [T/α₂]π₂ ⊫ₑ {π₁, τ₁ = [T/α₂]τ₂} ⤳! ∙ ; R
    * T new constructors
    * ---------------------------------------
    * Θₚ ⊩ (∀α₁.π₁ ⇒ τ₁) ≤ (∀α₂.π₂ ⇒ τ₂)
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, tenv0: Map[Symbol.TraitSym, Ast.TraitContext], eenv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {

    // Instantiate sc2, creating [T/α₂]π₂ and [T/α₂]τ₂
    val (cconstrs2_0, econstrs2_0, tpe2_0) = Scheme.instantiate(sc2, SourceLocation.Unknown)

    // Resolve what we can from the new econstrs
    // TODO ASSOC-TYPES probably these should be narrow from the start
    val tconstrs2_0 = econstrs2_0.map { case Ast.BroadEqualityConstraint(t1, t2) => TypeConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val (subst, econstrs2_1) = ConstraintSolver.resolve(tconstrs2_0, Substitution.empty, RigidityEnv.empty)(tenv0, eenv0, flix) match {
      case Result.Ok(ResolutionResult(newSubst, newConstrs, _)) =>
        (newSubst, newConstrs)
      case _ => throw InternalCompilerException("unexpected inconsistent type constraints", SourceLocation.Unknown)
    }

    // Anything we didn't solve must be a standard equality constraint
    // Apply the substitution to the new scheme 2
    val econstrs2 = econstrs2_1.map {
      case TypeConstraint.Equality(t1, t2, prov) => EqualityEnvironment.narrow(Ast.BroadEqualityConstraint(subst(t1), subst(t2)))
      case _ => throw InternalCompilerException("unexpected constraint", SourceLocation.Unknown)
    }
    val tpe2 = subst(tpe2_0)
    val cconstrs2 = cconstrs2_0.map {
      case Ast.TypeConstraint(head, arg, loc) =>
        // should never fail
        val (t, _) = TypeReduction.simplify(subst(arg), RigidityEnv.empty, loc)(eenv0, flix).get
        Ast.TypeConstraint(head, t, loc)
    }

    // Add sc2's constraints to the environment
    val eenv = ConstraintSolver.expandEqualityEnv(eenv0, econstrs2)
    val cenv = ConstraintSolver.expandTraitEnv(tenv0, cconstrs2)

    // Mark all the constraints from sc2 as rigid
    val tvars = cconstrs2.flatMap(_.arg.typeVars) ++
      econstrs2.flatMap { econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars } ++
      tpe2.typeVars
    val renv = tvars.foldLeft(RigidityEnv.empty) { case (r, tvar) => r.markRigid(tvar.sym) }

    // Check that the constraints from sc1 hold
    // And that the bases unify
    val cconstrs = sc1.tconstrs.map { case Ast.TypeConstraint(head, arg, loc) => TypeConstraint.Trait(head.sym, arg, loc) }
    val econstrs = sc1.econstrs.map { case Ast.BroadEqualityConstraint(t1, t2) => TypeConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val baseConstr = TypeConstraint.Equality(sc1.base, tpe2, Provenance.Match(sc1.base, tpe2, SourceLocation.Unknown))
    ConstraintSolver.resolve(baseConstr :: cconstrs ::: econstrs, subst, renv)(cenv, eenv, flix) match {
      // We succeed only if there are no leftover constraints
      case Result.Ok(ResolutionResult(_, Nil, _)) => true
      case _ => false
    }

  }

}

/**
  * Representation of polytypes.
  */
case class Scheme(quantifiers: List[Symbol.KindedTypeVarSym], tconstrs: List[Ast.TypeConstraint], econstrs: List[Ast.BroadEqualityConstraint], base: Type) {

  /**
    * Returns a human readable representation of the polytype.
    */
  override def toString: String = {
    FormatScheme.formatSchemeWithOptions(this, FormatOptions.Internal)
  }

}
