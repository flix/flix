package impl

trait Error

object Error {

  case class UnknownInterpretation(s: Symbol) extends RuntimeException(s"$s has no interpretation.")

  case class UnableToSatisfyPredicate(p: Predicate) extends RuntimeException

}
