package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

case class ScopeInfo(scopedness: Scopedness, scopeType: ScopeType)

object ScopeInfo {
  def freshVar()(implicit flix: Flix): ScopeInfo = ScopeInfo(Scopedness.freshVar(), ScopeType.freshVar())
}
