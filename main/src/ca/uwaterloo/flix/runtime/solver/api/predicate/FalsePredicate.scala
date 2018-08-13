package ca.uwaterloo.flix.runtime.solver.api.predicate

/**
  * Represents the false predicate.
  */
class FalsePredicate extends Predicate {
  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "false"
}
