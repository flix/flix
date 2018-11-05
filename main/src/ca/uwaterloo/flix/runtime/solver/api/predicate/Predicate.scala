package ca.uwaterloo.flix.runtime.solver.api.predicate

/**
  * A common super-type for predicates.
  */
trait Predicate {
  /**
    * Returns a copy of this predicate.
    */
  def copy(): Predicate
}
