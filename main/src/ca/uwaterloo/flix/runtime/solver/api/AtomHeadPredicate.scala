package ca.uwaterloo.flix.runtime.solver.api

// TODO: Should not be case class.
case class AtomHeadPredicate(sym: TableSym, terms: List[HeadTerm]) extends HeadPredicate {
  val arity: Int = terms.length
  val termsAsArray: Array[HeadTerm] = terms.toArray
}
