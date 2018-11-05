package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a table value, either a relation or lattice.
  */
trait Table {
  /**
    * Returns a copy of `this` table.
    */
  def copy(): Table

  /**
    * Returns the name of the table.
    */
  def getName(): String
}
