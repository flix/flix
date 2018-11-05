package ca.uwaterloo.flix.runtime.solver.api.predicate

/**
  * Represents the false predicate.
  */
class FalsePredicate extends Predicate {
  /**
    * Returns a copy of this predicate.
    */
  override def copy(): Predicate = new FalsePredicate()

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "false"
}
