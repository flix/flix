package ca.uwaterloo.flix.runtime.solver.api.term

/**
  * Represents a wildcard variable term.
  */
class WildTerm extends Term {
  /**
    * Returns a string representation of `this` term.
    */
  override def toString: String = "_"
}
