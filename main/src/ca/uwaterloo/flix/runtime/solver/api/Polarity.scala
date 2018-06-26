package ca.uwaterloo.flix.runtime.solver.api

sealed trait Polarity

object Polarity {

  case object Positive extends Polarity

  case object Negative extends Polarity

}
