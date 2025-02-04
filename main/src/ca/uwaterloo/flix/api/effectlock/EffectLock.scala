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
    unifiableSchemes(sc1, sc2) || isSubset(sc1.base, sc2.base)
  }

  /**
    * Generalize-rule
    *
    * 𝜎1 ⊑ 𝜏2
    * -------
    * 𝜎1 ⪯ 𝜏2
    *
    */
  private def unifiableSchemes(sc1: Scheme, sc2: Scheme): Boolean = {
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val renv = RigidityEnv.apply(SortedSet.from(sc2.quantifiers))
    val unification = Unification.fullyUnifyTypes(sc1.base, sc2.base, renv, EqualityEnv.empty)(Scope.Top, flix)
    unification.isDefined
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
    // TODO: What about type variables? Alpha equivalence
    // 1. Types match t1 -> t2
    (tpe1.typeConstructor, tpe2.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) => ()
      case _ => return false
    }
    val isMatchingArgs = tpe1.arrowArgTypes == tpe2.arrowArgTypes
    val tpe1Res = tpe1.arrowResultType
    val tpe2Res = tpe2.arrowResultType
    val isMatchingResultTypes = (tpe1.arrowResultType.typeConstructor, tpe2.arrowResultType.typeConstructor) match {
      case (Some(TypeConstructor.Arrow(_)), Some(TypeConstructor.Arrow(_))) => isSubset(tpe1Res, tpe2Res)
      case (t1, t2) => t1 == t2
    }


    // 2. Boolean unification of effects phi + phi' = phi'
    val sc1Effs = tpe1.arrowEffectType
    val sc2Effs = tpe2.arrowEffectType
    val left = Type.mkUnion(sc1Effs, sc2Effs, sc1Effs.loc)
    implicit val flix: Flix = new Flix().setOptions(Options.Default)
    val (unsolvedConstraints, _) = EffUnification3.unifyAll(List((left, sc2Effs, sc2Effs.loc)), Scope.Top, RigidityEnv.empty)
    isMatchingArgs && isMatchingResultTypes && unsolvedConstraints.isEmpty
  }
}
