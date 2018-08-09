package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.Table
import ca.uwaterloo.flix.runtime.solver.api.term.Term
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents an atom predicate for the symbol `sym` with arguments `terms`.
  */
class AtomPredicate(sym: Table, positive: Boolean, terms: Array[Term], index2sym: Array[VarSym]) extends Predicate {

  /**
    * Returns the table symbol of the atom.
    */
  def getSym(): Table = sym

  /**
    * Returns `true` if this atom is un-negated.
    */
  def isPositive(): Boolean = positive

  /**
    * Returns `true` if this atom is negated.
    */
  def isNegative(): Boolean = !positive

  /**
    * Returns the terms of the atom.
    */
  def getTerms(): Array[Term] = terms

  // TODO: Deprecated.
  def getIndex2SymTEMPORARY: Array[VarSym] = index2sym

  override def toString: String = s"$sym(${terms.mkString(", ")})"

}
