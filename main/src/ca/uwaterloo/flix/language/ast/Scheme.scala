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
import ca.uwaterloo.flix.language.phase.ConstraintResolution
import ca.uwaterloo.flix.language.phase.ConstraintResolution.ResolutionResult
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint.Provenance
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
  def instantiate(sc: Scheme, loc: SourceLocation)(implicit level: Level, flix: Flix): (List[Ast.TypeConstraint], List[Ast.BroadEqualityConstraint], Type) = {
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
      */
    def visitType(t: Type): Type = t match {
      case Type.Var(sym, _) => freshVars.getOrElse(sym.id, t)
      case Type.Cst(tc, _) => Type.Cst(tc, loc)
      case Type.Apply(tpe1, tpe2, _) => Type.Apply(visitType(tpe1), visitType(tpe2), loc)
      case Type.Alias(sym, args, tpe, _) => Type.Alias(sym, args.map(visitType), visitType(tpe), loc)
      case Type.AssocType(sym, args, kind, _) => Type.AssocType(sym, args.map(visitType), kind, loc)
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
  def equal(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    lessThanEqual(sc1, sc2, classEnv, eqEnv) && lessThanEqual(sc2, sc1, classEnv, eqEnv)
  }

  /**
    * Returns `true` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    */
  def lessThanEqual(sc1: Scheme, sc2: Scheme, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Boolean = {
    checkLessThanEqual(sc1, sc2, classEnv, eqEnv).toHardResult match {
      case Result.Ok(_) => true
      case Result.Err(_) => false
    }
  }

  /**
    * Returns `Success` if the given scheme `sc1` is smaller or equal to the given scheme `sc2`.
    *
    * Θₚ [T/α₂]π₂ ⊫ₑ {π₁, τ₁ = [T/α₂]τ₂} ⤳! ∙ ; R
    * T new constructors
    * ---------------------------------------
    * Θₚ ⊩ (∀α₁.π₁ ⇒ τ₁) ≤ (∀α₂.π₂ ⇒ τ₂)
    */
  def checkLessThanEqual(sc1: Scheme, sc2: Scheme, cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eenv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Validation[Substitution, UnificationError] = {
    // TODO ASSOC-TYPES probably these should be narrow from the start

    // Mark every free variable in `sc1` as rigid.
    //    val renv1 = RigidityEnv(sc1.base.typeVars.map(_.sym) -- sc1.quantifiers)

    // Instantiate sc2, creating [T/α₂]π₂ and [T/α₂]τ₂
    val (cconstrs2_0, econstrs2_0, tpe2_0) = Scheme.instantiate(sc2, SourceLocation.Unknown)(Level.Default, flix)

    // Resolve what we can from the new econstrs
    val tconstrs2_0 = econstrs2_0.map { case Ast.BroadEqualityConstraint(t1, t2) => TypingConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val (subst, econstrs2_1) = ConstraintResolution.resolve(tconstrs2_0, Substitution.empty, RigidityEnv.empty)(cenv0, eenv0, flix) match {
      case Result.Ok(ResolutionResult(newSubst, newConstrs, _)) =>
        (newSubst, newConstrs)
      case _ => throw InternalCompilerException("unexpected inconsistent type constraints", SourceLocation.Unknown)
    }

    // Anything we didn't solve must be a standard equality constraint
    // Apply the substitution to the new scheme 2
    val econstrs2 = econstrs2_1.map {
      case TypingConstraint.Equality(t1, t2, prov) => EqualityEnvironment.narrow(Ast.BroadEqualityConstraint(subst(t1), subst(t2)))
      case _ => throw InternalCompilerException("unexpected constraint", SourceLocation.Unknown)
    }
    val tpe2 = subst(tpe2_0)
    val cconstrs2 = cconstrs2_0.map {
      case Ast.TypeConstraint(head, arg, loc) =>
        // should never fail
        val (t, _) = ConstraintResolution.simplifyType(subst(arg), RigidityEnv.empty, eenv0, loc).get
        Ast.TypeConstraint(head, t, loc)
    }

    // Add sc2's constraints to the environment
    val eenv = ConstraintResolution.expandEqualityEnv(eenv0, econstrs2)
    val cenv = ConstraintResolution.expandClassEnv(cenv0, cconstrs2)

    // Mark all the constraints from sc2 as rigid
    val tvars = cconstrs2.flatMap(_.arg.typeVars) ++
      econstrs2.flatMap { econstr => econstr.tpe1.typeVars ++ econstr.tpe2.typeVars } ++
      tpe2.typeVars
    val renv = tvars.foldLeft(RigidityEnv.empty) { case (r, tvar) => r.markRigid(tvar.sym) }

    // Check that the constraints from sc1 hold
    // And that the bases unify
    val cconstrs = sc1.tconstrs.map { case Ast.TypeConstraint(head, arg, loc) => TypingConstraint.Class(head.sym, arg, loc) }
    val econstrs = sc1.econstrs.map { case Ast.BroadEqualityConstraint(t1, t2) => TypingConstraint.Equality(t1, t2, Provenance.Match(t1, t2, SourceLocation.Unknown)) }
    val baseConstr = TypingConstraint.Equality(sc1.base, tpe2, Provenance.Match(sc1.base, tpe2, SourceLocation.Unknown))
    ConstraintResolution.resolve(baseConstr :: cconstrs ::: econstrs, subst, renv)(cenv, eenv, flix) match {
      case Result.Ok(ResolutionResult(subst, Nil, _)) => Validation.success(subst)
      case Result.Ok(ResolutionResult(subst, leftovers, _)) =>
        println(subst)
        println(leftovers)
        Validation.HardFailure(Chain(UnificationError.MismatchedTypes(sc1.base, sc2.base)))
      // TODO ASSOC-TYPES better error stuff
      case _ => Validation.HardFailure(Chain(UnificationError.MismatchedTypes(sc1.base, sc2.base)))
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
