package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.term.Term

case class AtomBodyPredicate(sym: TableSym, polarity: Polarity, terms: Array[Term], index2sym: Array[VarSym]) extends BodyPredicate {
  val arity: Int = terms.length
}