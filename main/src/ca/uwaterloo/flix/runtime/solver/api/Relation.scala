package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

class Relation(sym: TableSym, attributes: Array[Attribute]) extends Table {

  /**
    * Returns the symbol of the relation.
    */
  def getSym(): TableSym = sym

  /**
    * Returns the attributes of the relation.
    */
  def getAttributes(): Array[Attribute] = attributes

}
