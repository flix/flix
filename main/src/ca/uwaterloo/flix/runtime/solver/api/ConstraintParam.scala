package ca.uwaterloo.flix.runtime.solver.api

class ConstraintParam(sym: VarSym) {

  /**
    * Returns the quantified variable symbol.
    */
  def getSymbol(): VarSym = sym

}
