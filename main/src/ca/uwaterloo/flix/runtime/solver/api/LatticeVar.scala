package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents an uninitialized lattice.
  *
  * @param name  the name of the lattice.
  * @param keys  the keys of the lattice.
  * @param value the value of the lattice.
  * @param ops   the lattice operations.
  */
class LatticeVar(name: String, keys: Array[Attribute], value: Attribute, ops: LatticeOps) extends Table {
  override def getName(): String = name

  def getKeys(): Array[Attribute] = keys

  def getValue(): Attribute = value

  def getOps(): LatticeOps = ops
}
