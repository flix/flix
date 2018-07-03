package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

class Lattice(sym: TableSym, keys: Array[Attribute], value: Attribute) extends Table {

  /**
    * Returns the symbol of the lattice.
    */
  def getSym(): TableSym = sym

  /**
    * Returns the key attributes of the lattice.
    */
  def getKeys(): Array[Attribute] = keys

  /**
    * Returns the value attribute of the lattice.
    */
  def getValue(): Attribute = value


}

