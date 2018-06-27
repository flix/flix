package ca.uwaterloo.flix.runtime.solver.api

case class AtomBodyPredicate(sym: TableSym, polarity: Polarity, terms: Array[BodyTerm], index2sym: Array[VarSym]) extends BodyPredicate {
  val arity: Int = terms.length
}