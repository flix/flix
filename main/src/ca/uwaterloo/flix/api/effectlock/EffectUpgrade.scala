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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, TypeConstraint}
import ca.uwaterloo.flix.language.phase.unification.{EffUnification3, EqualityEnv}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.ListOps

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object EffectUpgrade {

  private def debug(obj: Object): Unit = {
    println(s"[DEBUG] $obj")
  }

  /**
    * Returns `true` if `upgrade` is a safe upgrade of `original`.
    *
    * `upgrade` is a safe upgrade of `original` if at least one of the following holds:
    *   - `upgrade` is unifiable with `original`.
    *   - `upgrade` is a monomorphic downgrade of `original`.
    */
  def isEffSafeUpgrade(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = {
    debug(s"original = $original")
    debug(s"upgrade = $upgrade")
    // Alpha rename so equality of types can be done via `==`.
    val orig = alpha(original)
    val upgr = alpha(upgrade)
    debug(s"orig = $orig")
    debug(s"upgr = $upgr")
    val res1 = isGeneralizable(original, upgrade)
    debug(s"isGeneralizable(original, upgrade) = $res1")
    val res2 = isSubset(orig, upgr)
    debug(s"isSubset(orig, upgr) = $res2")
    res1 || res2
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜎2
    * -------
    * 𝜎1 ⪯ 𝜎2
    *
    */
  private def isGeneralizable(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = {
    implicit val eqEnv: EqualityEnv = EqualityEnv.empty
    val renv = RigidityEnv.apply(SortedSet.from(original.quantifiers))
    val unification = ConstraintSolver2.fullyUnify(original.base, upgrade.base, Scope.Top, renv)

    unification match {
      case Some(subst) =>
        original.base == subst(upgrade.base)

      case None =>
        false
    }
  }

  /**
    * `upgrade` is a safe upgrade of `original` if its effects is a subset of `original`:
    *
    * 𝜑 ∪ 𝜑' ≡ 𝜑
    * ----------
    * 𝜏1 −→ 𝜏2 \ 𝜑 ⪯ 𝜏1 -→ 𝜏2 \ 𝜑′
    *
    * `𝜑` are the effects of `original` and `𝜑'` are the effects of `upgrade`.
    *
    * Assumes that `original` and `upgrade` have been alpha-renamed so the variables have the same names if they are equal.
    */
  private def isSubset(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = {
    val res1 = isSameType(original, upgrade)
    val res2 = isEffectSubset(original, upgrade)
    debug(s"original = $original")
    debug(s"upgrade = $upgrade")
    debug(s"isSameType(original, upgrade) = $res1")
    debug(s"isEffectSubset(original, upgrade) = $res2")
    res1 && res2
  }

  /**
    * Checks that the type in `original` is the same type as `upgrade`.
    * Assumes that they have been alpha-renamed so the variables have the same names if they are equal.
    */
  private def isSameType(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = (original.base.typeConstructor, upgrade.base.typeConstructor) match {
    case (Some(TypeConstructor.Arrow(n01)), Some(TypeConstructor.Arrow(n02))) if n01 == n02 =>
      // Check same args
      val isSameArgs = ListOps.zip(original.base.arrowArgTypes, upgrade.base.arrowArgTypes).map {
        case (argTpe1, argTpe2) => (original.copy(base = argTpe1), upgrade.copy(base = argTpe2))
      }.forall { case (sc1, sc2) => isSubset(sc1, sc2) }

      // Check same result
      val isSameResult = isSubset(original.copy(base = original.base.arrowResultType), upgrade.copy(base = upgrade.base.arrowResultType))

      // Assert both hold
      isSameArgs && isSameResult

    case (_, _) =>
      // Base case: Non-arrow types. Directly compare for equality
      debug(s"Non-arrow type: $original")
      debug(s"Non-arrow type: $upgrade")
      original.base == upgrade.base
  }

  /**
    * Checks that the effects of `upgrade` is a subset of the effects of `original`.
    */
  private def isEffectSubset(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = (original.base.typeConstructor, upgrade.base.typeConstructor) match {
    case (Some(TypeConstructor.Arrow(n01)), Some(TypeConstructor.Arrow(n02))) if n01 == n02 =>
      val originalEff = original.base.arrowEffectType
      val upgradeEff = upgrade.base.arrowEffectType
      val union = Type.mkUnion(originalEff, upgradeEff, upgradeEff.loc)
      val renv = RigidityEnv.ofRigidVars(original.quantifiers)
      val provenance = TypeConstraint.Provenance.ExpectEffect(originalEff, union, originalEff.loc)
      val constraint = TypeConstraint.Equality(union, originalEff, provenance)
      EffUnification3.unifyAll(List(constraint), Scope.Top, renv) match {
        case Result.Ok(_) => true
        case Result.Err(_) => false
      }

    case (_, _) =>
      // Any non-arrow type has no effects so they are trivially always effect subsets of each other
      true
  }

  /**
    * Performs alpha-renaming on `sc0`.
    *
    * To account for change in signatures, symbols with different kinds are renamed differently.
    */
  private def alpha(sc0: Scheme): Scheme = {
    val seen = mutable.Map.empty[Kind, mutable.Map[Symbol.KindedTypeVarSym, Symbol.KindedTypeVarSym]]

    def visit(tpe0: Type): Type = tpe0 match {
      case Type.Var(sym, loc) =>
        if (!seen.contains(sym.kind)) {
          seen.put(sym.kind, mutable.Map.empty[Symbol.KindedTypeVarSym, Symbol.KindedTypeVarSym])
        }
        val innerMap = seen(sym.kind)
        innerMap.get(sym) match {
          case Some(subst) => Type.Var(subst, loc)
          case None =>
            val subst = new Symbol.KindedTypeVarSym(innerMap.size, sym.text, sym.kind, sym.isSlack, sym.scope, sym.loc)
            innerMap += sym -> subst
            Type.Var(subst, loc)
        }

      case Type.Cst(_, _) =>
        tpe0

      case Type.Apply(tpe1, tpe2, loc) =>
        Type.Apply(visit(tpe1), visit(tpe2), loc)

      case Type.Alias(symUse, args, tpe, loc) =>
        Type.Alias(symUse, args.map(visit), visit(tpe), loc)

      case Type.AssocType(symUse, arg, kind, loc) =>
        Type.AssocType(symUse, visit(arg), kind, loc)

      case Type.JvmToType(tpe, loc) =>
        Type.JvmToType(visit(tpe), loc)

      case Type.JvmToEff(tpe, loc) =>
        Type.JvmToEff(visit(tpe), loc)

      case Type.UnresolvedJvmType(_, _) =>
        tpe0
    }

    val base = visit(sc0.base)
    val tconstrs = sc0.tconstrs.map(tc => tc.copy(arg = visit(tc.arg)))
    val econstrs = sc0.econstrs.map(ec => ec.copy(tpe1 = visit(ec.tpe1), tpe2 = visit(ec.tpe2)))
    val qs = sc0.quantifiers.map {
      q =>
        seen.get(q.kind) match {
          case Some(inner) => inner.getOrElse(q, q)
          case None => q
        }
    }
    Scheme(qs, tconstrs, econstrs, base)
  }

}
