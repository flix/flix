package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

class VarTerm(sym: VarSym) extends Term {

  /**
    * Returns the variable symbol.
    */
  def getSym(): VarSym = sym

}
