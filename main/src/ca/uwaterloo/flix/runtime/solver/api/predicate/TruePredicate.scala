package ca.uwaterloo.flix.runtime.solver.api.predicate

/**
  * Represents the true predicate.
  */
class TruePredicate extends Predicate {
  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "true"
}
