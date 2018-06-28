package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.term.{Term, WildTerm}

class FilterPredicate(f: Array[AnyRef] => Boolean, terms: Array[Term]) extends Predicate {

  /**
    * Constructor.
    */
  for (t <- terms) {
    if (t.isInstanceOf[WildTerm]) {
      throw new IllegalArgumentException("Unexpected wildcard term.")
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

}
