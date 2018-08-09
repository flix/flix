package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

/**
  * Represents a variable term with the variable `sym`.
  */
class VarTerm(sym: VarSym) extends Term {

  /**
    * Returns the variable symbol.
    */
  def getSym(): VarSym = sym

  /**
    * Returns a string representation of `this` term.
    */
  override def toString: String = sym.toString

}
