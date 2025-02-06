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

  def println(s: Object): Unit = {
    if (false) {
      Predef.println(s)
    }
  }

  /**
    * Returns true if `sc1` is unifiable with `sc2` or if `sc1` is a monomorphic downgrade of `sc2`.
    */
  def isSafe(sc1: Scheme, sc2: Scheme): Boolean = {
    isGeneralizable(sc1, sc2) || isSubset(sc1.base, sc2.base)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def isGeneralizable(sc1: Scheme, sc2: Scheme): Boolean = {
    println(s"g: $sc1")
    println(s"f: $sc2")
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, renv, EqualityEnv.empty)(Scope.Top, flix)
    unification match {
      case Some(subst) =>
        println(subst)
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
  private def isSubset(tpe1: Type, tpe2: Type): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    // TODO: What about type variables? Alpha equivalence
    // 1. Types match t1 -> t2
    (tpe1.typeConstructor, tpe2.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) => ()
      case _ => return false
    }
    val isMatchingArgs = true // tpe1.arrowArgTypes == tpe2.arrowArgTypes
    println(s"tpe1.arrowArgTypes: ${tpe1.arrowArgTypes}")
    println(s"tpe2.arrowArgTypes: ${tpe2.arrowArgTypes}")
    println(s"isMatchingArgs: $isMatchingArgs")
    val tpe1Res = tpe1.arrowResultType
    val tpe2Res = tpe2.arrowResultType
    val isMatchingResultTypes = (tpe1.arrowResultType.typeConstructor, tpe2.arrowResultType.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) => isSubset(tpe1Res, tpe2Res)
      case (t1, t2) =>
        println(s"t1: $t1")
        println(s"t2: $t2")
        t1 == t2
    }
    println(s"tpe1.arrowResultType: ${tpe1.arrowResultType}")
    println(s"tpe2.arrowResultType: ${tpe2.arrowResultType}")
    println(s"isMatchingResultTypes: $isMatchingResultTypes")


    // 2. Boolean unification of effects phi + phi' = phi'
    val sc1Effs = tpe1.arrowEffectType
    val sc2Effs = tpe2.arrowEffectType
    println(s"g sc1Effs: $sc1Effs")
    println(s"f sc2Effs: $sc2Effs")
    val left = Type.mkUnion(sc1Effs, sc2Effs, sc1Effs.loc)
    val (unsolvedConstraints, subst) = EffUnification3.unifyAll(List((left, sc2Effs, sc2Effs.loc)), Scope.Top, RigidityEnv.empty)
    println(subst)
    println(unsolvedConstraints)
    val isUnifiableForAllValuations = unsolvedConstraints.isEmpty && subst.isEmpty
    isMatchingArgs && isMatchingResultTypes & unsolvedConstraints.isEmpty // && isUnifiableForAllValuations
  }
}

