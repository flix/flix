package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.LatticeOps

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

  def getOps(): LatticeOps = ops

}
