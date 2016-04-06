package ca.uwaterloo.flix.language.ast

sealed trait Law

object Law {

  case object Associativity extends Law

  case object Commutativity extends Law

  case object Reflexivity extends Law

  case object AntiSymmetry extends Law

  case object Transitivity extends Law

  case object LeastElement extends Law

  case object UpperBound extends Law

  case object LeastUpperBound extends Law

  case object GreatestElement extends Law

  case object LowerBound extends Law

  case object GreatestLowerBound extends Law

  case object Strict extends Law

  case object Monotone extends Law

  case object HeightNonNegative extends Law

  case object HeightStrictlyDecreasing extends Law

}
