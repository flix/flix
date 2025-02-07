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
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{EffUnification3, EqualityEnv, Unification}
import ca.uwaterloo.flix.util.Options

import scala.collection.immutable.SortedSet

object EffectLock {

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    isGeneralizable(sc1, sc2) || isSubset(sc1, sc2)
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
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, renv, EqualityEnv.empty)(Scope.Top, flix)
    unification match {
      case Some(subst) => subst(sc1.base) == sc2.base
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
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) => ()
      case _ => return false
    }
    val isMatchingArgs = true // tpe1.arrowArgTypes == tpe2.arrowArgTypes
    val tpe1Res = tpe1.arrowResultType
    val tpe2Res = tpe2.arrowResultType
    val isMatchingResultTypes = (tpe1.arrowResultType.typeConstructor, tpe2.arrowResultType.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) =>
        val sc11 = Scheme(sc1.quantifiers, List.empty, List.empty, tpe1Res)
        val sc21 = Scheme(sc2.quantifiers, List.empty, List.empty, tpe2Res)
        isSubset(sc11, sc21)

      case (t1, t2) =>
        t1 == t2
    }

    // 2. Boolean unification of effects phi + phi' = phi'
    val sc1Effs = tpe1.arrowEffectType
    val sc2Effs = tpe2.arrowEffectType
    val left = Type.mkUnion(sc1Effs, sc2Effs, sc1Effs.loc)
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val (unsolvedConstraints, _) = EffUnification3.unifyAll(List((left, sc2Effs, sc2Effs.loc)), Scope.Top, renv)
    isMatchingArgs && isMatchingResultTypes & unsolvedConstraints.isEmpty
  }
}

