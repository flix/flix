package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

class ConstraintParam(sym: VarSym) {

  /**
    * Returns the quantified variable symbol.
    */
  def getSymbol(): VarSym = sym

}
