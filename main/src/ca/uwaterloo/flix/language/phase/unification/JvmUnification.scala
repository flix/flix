package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Type}

object JvmUnification {

  // MATT docs
  def unify(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit scope: Scope): Option[Substitution] = (tpe1, tpe2) match {
    case (Type.Var(sym, _), t2) if renv.isFlexible(sym) => Some(Substitution.singleton(sym, t2))
    case (t1, Type.Var(sym, _)) if renv.isFlexible(sym) => Some(Substitution.singleton(sym, t1))
    case _ => None
  }

}
