package ca.uwaterloo.flix.runtime.solver.api

// TODO: Should not be case class.
case class AtomHeadPredicate(sym: TableSym, terms: List[Term.Head]) extends HeadPredicate {
  val arity: Int = terms.length
  val termsAsArray: Array[Term.Head] = terms.toArray
}
