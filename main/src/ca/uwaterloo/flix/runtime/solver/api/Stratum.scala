package ca.uwaterloo.flix.runtime.solver.api

/**
  * Represents a stratum.
  */
class Stratum(constraints: Array[Constraint]) {

  /**
    * Returns the constraints in the stratum.
    */
  def getConstraints(): Array[Constraint] = constraints

  /**
    * Returns a string representation of `this` stratum.
    */
  override def toString: String = "{\n\t" + constraints.mkString(", \n\t") + "\n}"

}
