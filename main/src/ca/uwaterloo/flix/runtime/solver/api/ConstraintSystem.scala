package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

case class ConstraintSystem(strata: List[Stratum], tables: Map[TableSym, Table], latOps: Map[TableSym, LatticeOps])
