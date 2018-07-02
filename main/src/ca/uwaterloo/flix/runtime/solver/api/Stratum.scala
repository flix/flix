package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a stratum.
  */
class Stratum(constraints: List[Constraint]) {

  /**
    * Returns the constraints in the stratum.
    */
  def getConstraints(): List[Constraint] = constraints

}
