package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

@deprecated
sealed trait ScopeScheme

@deprecated
object ScopeScheme {
  case object Unit extends ScopeScheme

  case class Arrow(sc1: ScopeInfo, sc2: ScopeInfo) extends ScopeScheme

  case class Tuple(scs: List[ScopeInfo]) extends ScopeScheme

  case class Record(field: Name.Field, sc: ScopeInfo, rest: ScopeInfo) extends ScopeScheme

  case class Var(id: Int) extends ScopeScheme

  def freshVar()(implicit flix: Flix): Var = Var(flix.genSym.freshId())
}
