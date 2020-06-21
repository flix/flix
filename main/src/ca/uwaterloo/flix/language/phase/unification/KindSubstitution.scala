package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

// MATT docs, license
case class KindSubstitution(m: Map[Kind.Var, Kind])(implicit flix: Flix) {

  // MATT docs
  // MATT empty check for efficiency
  def makeTypeSubst(t: Type): Substitution = t match {
    case tvar@Type.Var(_, kvar: Kind.Var, _) if m contains kvar =>
      Substitution.singleton(tvar, Type.freshTypeVar(m(kvar))) // MATT maybe check for repeats
    case Type.Apply(tpe1, tpe2) => makeTypeSubst(tpe1) ++ makeTypeSubst(tpe2)
    case Type.Arrow(_, eff) => makeTypeSubst(eff)
    case Type.Cst(_) => Substitution.empty
    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type: $t")
  }
}

object KindSubstitution {
  // MATT check for equality
  def singleton(kind1: Kind.Var, kind2: Kind)(implicit flix: Flix): KindSubstitution = KindSubstitution(Map(kind1 -> kind2))

  // MATT try to get rid of flix param
  // MATT change to singleton val
  def empty(implicit flix: Flix): KindSubstitution = KindSubstitution(Map.empty)
}
