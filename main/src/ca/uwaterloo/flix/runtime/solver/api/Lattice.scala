package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.datastore.IndexedLattice
import ca.uwaterloo.flix.util.BitOps

/**
  * Represents a lattice value.
  */
class Lattice(name: String, keys: Array[Attribute], value: Attribute, ops: LatticeOps) extends Table {

  /**
    * Returns the name of the lattice.
    */
  def getName(): String = name

  /**
    * Returns the key attributes of the lattice.
    */
  def getKeys(): Array[Attribute] = keys

  /**
    * Returns the value attribute of the lattice.
    */
  def getValue(): Attribute = value

  private val indexedLattice = {
    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedLattice(keys, value, indexes, ops)
  }

  def getIndexedLattice(): IndexedLattice = indexedLattice

  /**
    * Returns a string representation of the lattice.
    */
  override def toString: String = s"$name(${indexedLattice.getSize})"

}
