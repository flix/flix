package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a relation or lattice attribute.
  */
class Attribute(name: String) {

  /**
    * Returns the name of the attribute.
    */
  def getName(): String = name

}
