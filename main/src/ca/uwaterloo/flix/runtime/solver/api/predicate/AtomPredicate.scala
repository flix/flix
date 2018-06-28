package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.polarity.Polarity
import ca.uwaterloo.flix.runtime.solver.api.term.Term
import ca.uwaterloo.flix.runtime.solver.api.symbol.{TableSym, VarSym}

case class AtomPredicate(sym: TableSym, polarity: Polarity, terms: Array[Term], index2sym: Array[VarSym]) extends Predicate {
  val arity: Int = terms.length
}
