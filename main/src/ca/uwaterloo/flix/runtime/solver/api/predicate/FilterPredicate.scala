package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.term.{Term, WildTerm}

/**
  * Represents a filter predicate with function `f` and arguments `terms`.
  */
class FilterPredicate(f: Array[AnyRef] => Boolean, terms: Array[Term]) extends Predicate {

  /**
    * Invariant: A filter predicate cannot have wild card terms as arguments.
    */
  for (t <- terms) {
    if (t.isInstanceOf[WildTerm]) {
      throw new IllegalArgumentException("A filter predicate cannot take a wild card term as an argument.")
    }
  }

  /**
    * Returns the filter function.
    */
  def getFunction(): Array[AnyRef] => Boolean = f

  /**
    * Returns the arguments of the filter function.
    */
  def getArguments(): Array[Term] = terms

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "<<filter>>" + "(" + terms.mkString(", ") + ")"

}
