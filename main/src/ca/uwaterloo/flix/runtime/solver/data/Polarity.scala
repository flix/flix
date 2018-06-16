package ca.uwaterloo.flix.runtime.solver.data

sealed trait Polarity

object Polarity {

  case object Positive extends Polarity

  case object Negative extends Polarity

}
