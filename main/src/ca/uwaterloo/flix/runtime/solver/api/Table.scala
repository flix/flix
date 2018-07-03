package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

trait Table {

  /**
    * Returns the symbol of the table.
    */
  def getSym(): TableSym

}
