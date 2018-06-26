package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps

case class ConstraintSystem(strata: List[Stratum], tables: Map[TableSym, Table], latOps: Map[TableSym, LatticeOps])