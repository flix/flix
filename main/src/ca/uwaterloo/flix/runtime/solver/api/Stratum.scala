package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a stratum.
  */
class Stratum(constraints: Array[Constraint]) {

  /**
    * Returns the constraints in the stratum.
    */
  def getConstraints(): Array[Constraint] = constraints

  override def toString: String = "{" + constraints.mkString(", ") + "}"

}
