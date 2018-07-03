package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.symbol.{LatSym, RelSym, TableSym}

/**
  * Represents a collection of constraints.
  */
class ConstraintSet(relSyms: Set[RelSym], latSyms: Set[LatSym], strata: List[Stratum], tables: Map[TableSym, Table], latOps: Map[TableSym, LatticeOps]) {

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

  /**
    * Returns the tables in the constraint set.
    */
  def getTables(): Map[TableSym, Table] = tables

  /**
    * Returns the lattice operations in the constraint set.
    */
  def getLatticeOps(): Map[TableSym, LatticeOps] = latOps

}
