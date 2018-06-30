package ca.uwaterloo.flix.runtime.solver.api.predicate

import ca.uwaterloo.flix.runtime.solver.api.polarity.Polarity
import ca.uwaterloo.flix.runtime.solver.api.term.Term
import ca.uwaterloo.flix.runtime.solver.api.symbol.{TableSym, VarSym}

class AtomPredicate(sym: TableSym, polarity: Polarity, terms: Array[Term], index2sym: Array[VarSym]) extends Predicate {

  /**
    * Returns the table symbol of the atom.
    */
  def getSym: TableSym = sym

  /**
    * Returns the polarity of the atom.
    */
  def getPolarity: Polarity = polarity

  /**
    * Returns the terms of the atom.
    */
  def getTerms: Array[Term] = terms

  /**
    * Returns the number of terms in the atom.
    */
  def getNumberOfTerms: Int = terms.length

  // TODO: Deprecated.
  def getIndex2SymTEMPORARY: Array[VarSym] = index2sym

}
