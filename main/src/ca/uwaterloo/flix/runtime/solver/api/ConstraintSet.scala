package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relSyms: Set[Relation], latSyms: Set[Lattice], strata: List[Stratum]) {

  /**
    * Returns the relation symbols in the constraint set.
    */
  def getRelSyms(): Set[Relation] = relSyms

  /**
    * Returns the lattice symbols in the constraint set.
    */
  def getLatSyms(): Set[Lattice] = latSyms

  /**
    * Returns the strata in the constraint set.
    */
  def getStrata(): List[Stratum] = strata

}
