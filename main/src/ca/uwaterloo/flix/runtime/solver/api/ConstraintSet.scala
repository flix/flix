package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.symbol.{LatSym, RelSym}

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relSyms: Set[RelSym], latSyms: Set[LatSym], strata: List[Stratum]) {

  /**
    * Returns the relation symbols in the constraint set.
    */
  def getRelSyms(): Set[RelSym] = relSyms

  /**
    * Returns the lattice symbols in the constraint set.
    */
  def getLatSyms(): Set[LatSym] = latSyms

  /**
    * Returns the strata in the constraint set.
    */
  def getStrata(): List[Stratum] = strata

}
