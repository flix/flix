package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix


sealed trait ScopeScheme

object ScopeScheme {
  case object Unit extends ScopeScheme

  case class Arrow(sc1: ScopeScheme, sc2: ScopeScheme) extends ScopeScheme

  case class NAry(scs: List[ScopeScheme]) extends ScopeScheme

  case class Var(id: Int) extends ScopeScheme

  def freshVar()(implicit flix: Flix): Var = Var(flix.genSym.freshId())
}
