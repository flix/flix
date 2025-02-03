package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Scheme}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, Unification}

object EffectLock {
  def isSafe(sc1: Scheme, sc2: Scheme)(implicit flix: Flix): Boolean = {
    Unification.fullyUnifyTypes(sc1.base, sc2.base, RigidityEnv.empty, EqualityEnv.empty)(Scope.Top, flix).isDefined
  }
}
