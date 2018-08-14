package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.api.term.{Term, WildTerm}

/**
  * Represents a functional predicate with variable `sym`, function `f`, and arguments `terms`.
  */
class FunctionalPredicate(sym: VarSym, f: Array[AnyRef] => Array[AnyRef], terms: Array[Term]) extends Predicate {

  /**
    * Invariant: A functional predicate cannot have wild card terms as arguments.
    */
  for (t <- terms) {
    if (t.isInstanceOf[WildTerm]) {
      throw new IllegalArgumentException("A functional predicate cannot take a wild card term as an argument.")
    }
  }

  /**
    * Returns the variable sym.
    */
  def getVarSym(): VarSym = sym

  /**
    * Returns the function.
    */
  def getFunction(): Array[AnyRef] => Array[AnyRef] = f

  /**
    * Returns the arguments.
    */
  def getArguments(): Array[Term] = terms

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = "<<functional>>" + "(" + terms.mkString(", ") + ")"

}
