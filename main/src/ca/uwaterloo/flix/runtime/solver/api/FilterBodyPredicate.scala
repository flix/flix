package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.term.Term

class FilterBodyPredicate(f: Array[AnyRef] => Boolean, terms: Array[Term]) extends BodyPredicate {

  // TODO: The terms cannot be wildcard terms.

  /**
    * Returns the filter function.
    */
  def getFunction(): Array[AnyRef] => Boolean = f

  /**
    * Returns the arguments of the filter function.
    */
  def getArguments(): Array[Term] = terms

}
