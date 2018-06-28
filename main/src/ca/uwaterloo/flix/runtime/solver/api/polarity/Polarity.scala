package ca.uwaterloo.flix.runtime.solver.api.polarity

trait Polarity {

  /**
    * Returns `true` if the associated atom is not negated.
    */
  def isPositive: Boolean = this match {
    case _: PositivePolarity => true
    case _: NegativePolarity => false
    case _ => throw new RuntimeException()
  }

  /**
    * Returns `true` if the associated atom is negated.
    */
  def isNegative: Boolean = !isPositive

}
