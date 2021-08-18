package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

sealed trait ScopeType {
  def asScoped: ScopeInfo = this.withScope(Scopedness.Scoped)

  def asUnscoped: ScopeInfo = this.withScope(Scopedness.Unscoped)

//  def scopeIrrelevant(implicit flix: Flix): ScopeInfo = this.withScope(Scopedness.freshVar())

  private def withScope(sc: Scopedness): ScopeInfo = ScopeInfo(sc, this)
}

object ScopeType {
  case class Var(id: Int) extends ScopeType

  case class Cst(cst: TypeConstructor) extends ScopeType

  case class Apply(s1: ScopeInfo, s2: ScopeInfo) extends ScopeType

  case class Lambda(svar: ScopeInfo, s: ScopeInfo) extends ScopeType


  def mkApply(base: ScopeInfo, ts: List[ScopeInfo]): ScopeInfo = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t).asUnscoped
  }
}
