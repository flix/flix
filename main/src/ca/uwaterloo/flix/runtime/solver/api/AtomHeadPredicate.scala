package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.term.{Term, WildTerm}

// TODO: Should not be case class.
case class AtomHeadPredicate(sym: TableSym, terms: List[Term]) extends HeadPredicate {
  val arity: Int = terms.length
  val termsAsArray: Array[Term] = terms.toArray

  assert(!terms.exists(_.isInstanceOf[WildTerm]))
}
