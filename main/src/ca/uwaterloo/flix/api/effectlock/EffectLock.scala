/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol.KindedTypeVarSym
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, TypeConstraint}
import ca.uwaterloo.flix.language.phase.unification.{EffUnification3, EqualityEnv}
import ca.uwaterloo.flix.util.{Options, Result}

import scala.collection.immutable.SortedSet

object EffectLock {
  private def debug(s: Object): Unit = {
    if (false) {
      println(s)
    }
  }

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val sc1rename = naiveAlphaRename(sc1)
    val sc2rename = naiveAlphaRename(sc2)
    debug(s"g: $sc1")
    debug(s"f: $sc2")
    debug(s"renamed sc1: $sc1rename")
    debug(s"renamed sc2: $sc2rename")
    isGeneralizable(sc1, sc2) || isSubset(sc1rename, sc2rename)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def isGeneralizable(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val unification = ConstraintSolver2.fullyUnify(sc1.base, sc2.base, Scope.Top, renv)(EqualityEnv.empty, flix)
    unification match {
      case Some(subst) =>
        debug(subst)
        debug(s"subst(sc1.base): ${subst(sc1.base)}")
        debug(s"sc2.base: ${sc2.base}")
        subst(sc1.base) == sc2.base
      case None => false
    }
  }

  /**
    * Subset-rule
    *
    * 𝜑 ∪ 𝜑′ ≡ 𝜑′
    * ----------
    * 𝜏1 −→ 𝜏2 \ 𝜑 ⪯ 𝜏1 -→ 𝜏2 \ 𝜑′
    *
    */
  private def isSubset(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    val tpe1 = sc1.base
    val tpe2 = sc2.base

    // 1. Types match t1 -> t2
    (tpe1.typeConstructor, tpe2.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) =>

        val isMatchingArgs = tpe1.arrowArgTypes == tpe2.arrowArgTypes
        debug(s"tpe1.arrowArgTypes: ${tpe1.arrowArgTypes}")
        debug(s"tpe2.arrowArgTypes: ${tpe2.arrowArgTypes}")
        debug(s"isMatchingArgs: $isMatchingArgs")
        val tpe1Res = tpe1.arrowResultType
        val tpe2Res = tpe2.arrowResultType
        val isMatchingResultTypes = (tpe1.arrowResultType.typeConstructor, tpe2.arrowResultType.typeConstructor) match {
          case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) =>
            val sc11 = Scheme(sc1.quantifiers, List.empty, List.empty, tpe1Res)
            val sc21 = Scheme(sc2.quantifiers, List.empty, List.empty, tpe2Res)
            isSubset(sc11, sc21)

          case (t1, t2) =>
            debug(s"t1: $t1")
            debug(s"t2: $t2")
            t1 == t2
        }
        debug(s"tpe1.arrowResultType: ${tpe1.arrowResultType}")
        debug(s"tpe2.arrowResultType: ${tpe2.arrowResultType}")
        debug(s"isMatchingResultTypes: $isMatchingResultTypes")


        // 2. Boolean unification of effects phi_upgrd + phi_orig = phi_orig
        val sc1Effs = tpe1.arrowEffectType
        val originalEffects = tpe2.arrowEffectType
        val upgradeEffs = Type.mkUnion(sc1Effs, originalEffects, sc1Effs.loc)
        val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
        val provenance = TypeConstraint.Provenance.ExpectEffect(originalEffects, upgradeEffs, originalEffects.loc)
        val constraint = TypeConstraint.Equality(upgradeEffs, originalEffects, provenance)
        EffUnification3.unifyAll(List(constraint), Scope.Top, renv) match {
          case Result.Ok(_) =>
            debug(s"sc1Effs: $sc1Effs")
            debug(s"sc2Effs: $originalEffects")
            isMatchingArgs && isMatchingResultTypes
          case Result.Err(unsolvedConstraints) =>
            debug(unsolvedConstraints)
            false
        }


      case _ => false
    }
  }

  private def naiveAlphaRename(sc: Scheme): Scheme = {
    val seen = scala.collection.mutable.Map.empty[KindedTypeVarSym, KindedTypeVarSym]

    def visit(tpe00: Type): Type = tpe00 match {
      case Type.Var(sym, loc) => seen.get(sym) match {
        case Some(subst) => Type.Var(subst, loc)
        case None =>
          val subst = new KindedTypeVarSym(seen.size, sym.text, sym.kind, sym.isSlack, sym.scope, sym.loc)
          seen += sym -> subst
          Type.Var(subst, loc)
      }

      case Type.Cst(tc, loc) =>
        Type.Cst(tc, loc)

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = visit(tpe1)
        val t2 = visit(tpe2)
        Type.Apply(t1, t2, loc)

      case Type.Alias(symUse, args, tpe, loc) =>
        val as = args.map(visit)
        val t = visit(tpe)
        Type.Alias(symUse, as, t, loc)

      case Type.AssocType(symUse, arg, kind, loc) =>
        val a = visit(arg)
        Type.AssocType(symUse, a, kind, loc)

      case Type.JvmToType(tpe, loc) =>
        val t = visit(tpe)
        Type.JvmToType(t, loc)

      case Type.JvmToEff(tpe, loc) =>
        val t = visit(tpe)
        Type.JvmToEff(t, loc)

      case Type.UnresolvedJvmType(member, loc) =>
        Type.UnresolvedJvmType(member, loc)
    }

    val base = visit(sc.base)
    val qs = sc.quantifiers.map(q => seen.getOrElse(q, q))
    Scheme(qs, sc.tconstrs, sc.econstrs, base)
  }
}

