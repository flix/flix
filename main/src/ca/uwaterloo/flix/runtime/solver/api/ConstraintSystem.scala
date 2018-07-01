package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

class ConstraintSystem(strata: List[Stratum], val tables: Map[TableSym, Table], val latOps: Map[TableSym, LatticeOps]) {

  /**
    * Returns the list of strata.
    */
  def getStrata(): List[Stratum] = strata



}
