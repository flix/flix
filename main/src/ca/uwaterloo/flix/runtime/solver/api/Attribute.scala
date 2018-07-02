package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a relation or lattice attribute.
  */
// TODO: Determine if this class can be removed.
class Attribute(name: String) {

  /**
    * Returns the name of the attribute.
    */
  def getName(): String = name

}
