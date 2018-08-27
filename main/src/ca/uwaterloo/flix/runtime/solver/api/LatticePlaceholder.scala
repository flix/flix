package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps

class LatticePlaceholder(name: String, val keys: Array[Attribute], val value: Attribute, val ops: LatticeOps) extends Table {
  override def getName(): String = name
}
