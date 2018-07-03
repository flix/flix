package ca.uwaterloo.flix.runtime.solver.api.symbol

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.Attribute
import ca.uwaterloo.flix.runtime.solver.datastore.IndexedLattice
import ca.uwaterloo.flix.util.BitOps

class LatSym(val name: String, val keys: Array[Attribute], val value: Attribute, ops: LatticeOps) extends TableSym {

  private val indexedLattice = {
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedLattice(keys, value, indexes, ops)
  }

  def getIndexedLattice(): IndexedLattice = indexedLattice

  def canEqual(other: Any): Boolean = other.isInstanceOf[LatSym]

  override def equals(other: Any): Boolean = other match {
    case that: LatSym =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}
