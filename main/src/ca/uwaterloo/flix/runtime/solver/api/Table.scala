package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a table value, either a relation or lattice.
  */
trait Table {
  /**
    * Returns the name of the table.
    */
  def getName(): String
}
