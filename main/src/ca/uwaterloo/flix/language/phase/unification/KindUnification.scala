package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Kind
import ca.uwaterloo.flix.util.Result

object KindUnification {

  def unify(kind1: Kind, kind2: Kind)(implicit flix: Flix): Result[KindSubstitution, UnificationError] = (kind1, kind2) match {
    case (k1: Kind.Var, k2) => Result.Ok(KindSubstitution.singleton(k1, k2))
    case (k1, k2: Kind.Var) => Result.Ok(KindSubstitution.singleton(k2, k1))
    case (k1, k2) if k1 == k2 => Result.Ok(KindSubstitution.empty)
//    case _ => Result.Err(UnificationError.MismatchedKinds()) // MATT make this
  }
}
