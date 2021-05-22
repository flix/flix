package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Kind

sealed trait KindUnificationError

object KindUnificationError {
  case class MismatchedKinds(k1: Kind, k2: Kind) extends KindUnificationError
}
