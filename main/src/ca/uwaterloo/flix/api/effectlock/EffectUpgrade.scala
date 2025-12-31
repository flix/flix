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
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, TypeConstraint}
import ca.uwaterloo.flix.language.phase.unification.{EffUnification3, EqualityEnv}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.ListOps

import scala.collection.immutable.SortedSet

object EffectUpgrade {

  /**
    * Returns `true` if `upgrade` is a safe upgrade of `original`.
    *
    * `upgrade` is a safe upgrade of `original` if at least one of the following holds:
    *   - `upgrade` is unifiable with `original`.
    *   - `upgrade` is a monomorphic downgrade of `original`.
    */
  def isEffSafeUpgrade(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = {
    // Alpha rename so equality of types can be done via `==`.
    val originalErased = Util.erase(original)
    val upgradeErased = Util.erase(upgrade)
    val orig = Util.alpha(originalErased)
    val upgr = Util.alpha(upgradeErased)
    isGeneralizable(originalErased, upgradeErased) || isSubset(orig, upgr)
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
      case Some(subst) => original.base == subst(upgrade.base)
      case None => false
    }
  }

  /**
    * `upgrade` is a safe upgrade of `original` if its effects is a subset of `original`:
    *
    * 𝜑 ∪ 𝜑' ≡ 𝜑
    * ----------
    * 𝜏1 −→ 𝜏2 \ 𝜑' ⪯ 𝜏1 -→ 𝜏2 \ 𝜑
    *
    * `𝜑` are the effects of `original` and `𝜑'` are the effects of `upgrade`.
    *
    * Assumes that `original` and `upgrade` have been alpha-renamed so the variables have the same names if they are equal.
    */
  private def isSubset(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = {
    isSameType(original, upgrade) && isEffectSubset(original, upgrade)
  }

  /**
    * Checks that the type in `original` is the same type as `upgrade`.
    * Assumes that they have been alpha-renamed so the variables have the same names if they are equal.
    */
  private def isSameType(original: Scheme, upgrade: Scheme)(implicit flix: Flix): Boolean = (original.base.typeConstructor, upgrade.base.typeConstructor) match {
    case (Some(TypeConstructor.Arrow(n01)), Some(TypeConstructor.Arrow(n02))) if n01 == n02 =>
      isSameArgTypes(original, upgrade) && isSameResultType(original, upgrade)

    case (_, _) =>
      // Base case: Non-arrow types. Directly compare types for equality
      original.base == upgrade.base
  }

  /**
    * Checks the argument types of schemes `original` and `upgrade` for equality,
    * assuming that they are both arrow types of same arity.
    */
  private def isSameArgTypes(original: Scheme, upgrade: Scheme) = {
    ListOps.zip(original.base.arrowArgTypes, upgrade.base.arrowArgTypes).map {
      case (argTpe1, argTpe2) => (original.copy(base = argTpe1), upgrade.copy(base = argTpe2))
    }.forall { case (argOrig, argUpgr) => argOrig == argUpgr }
  }

  /**
    * Checks that [[isSubset]] holds for the result types of `original` and `upgrade`,
    * assuming that they are both arrow types of same arity.
    */
  private def isSameResultType(original: Scheme, upgrade: Scheme)(implicit flix: Flix) = {
    (original.base.arrowResultType.typeConstructor, upgrade.base.arrowResultType.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(n03)), Some(TypeConstructor.Arrow(n04))) if n03 == n04 =>
        // Check relation holds recursively if result is a function
        isSubset(original.copy(base = original.base.arrowResultType), upgrade.copy(base = upgrade.base.arrowResultType))

      case (_, _) => original.base.arrowResultType == upgrade.base.arrowResultType
    }
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
      // Any non-arrow type has no effects so their effect sets do not exist and cannot be subsets of one another.
      false
  }

}
