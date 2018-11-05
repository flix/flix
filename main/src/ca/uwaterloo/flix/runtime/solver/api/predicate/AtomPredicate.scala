package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.Table
import ca.uwaterloo.flix.runtime.solver.api.term.Term
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents an atom predicate for the symbol `sym` with arguments `terms`.
  */
class AtomPredicate(val sym: Table, positive: Boolean, terms: Array[Term], index2sym: Array[VarSym]) extends Predicate {

  /**
    * Returns a copy of this predicate.
    */
  override def copy(): Predicate = new AtomPredicate(sym.copy(), positive, terms, index2sym)

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

  /**
    * Returns a string representation of `this` predicate.
    */
  override def toString: String = sym.getName() + "(" + terms.mkString(", ") + ")"

  def canEqual(other: Any): Boolean = other.isInstanceOf[AtomPredicate]

  override def equals(other: Any): Boolean = other match {
    case that: AtomPredicate =>
      (that canEqual this) &&
        sym == that.sym
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sym)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
