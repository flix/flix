package ca.uwaterloo.flix.language.ast

case class ScopeInfo(sc: Scopedness, scSc: ScopeScheme) {
  def asScoped: ScopeInfo = copy(sc = Scopedness.Scoped)
}

object ScopeInfo {
  val UnscopedUnit: ScopeInfo = ScopeInfo(Scopedness.Unscoped, ScopeScheme.Unit)
}

