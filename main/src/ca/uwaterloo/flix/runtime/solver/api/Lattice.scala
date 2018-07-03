package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.datastore.IndexedLattice
import ca.uwaterloo.flix.util.BitOps

/**
  * Represents a lattice value.
  */
class Lattice(val name: String, val keys: Array[Attribute], val value: Attribute, ops: LatticeOps) extends Table {

  private val indexedLattice = {
    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedLattice(keys, value, indexes, ops)
  }

  def getIndexedLattice(): IndexedLattice = indexedLattice

  override def toString: String = s"$name(${indexedLattice.getSize})"

}
