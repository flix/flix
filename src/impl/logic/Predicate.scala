package impl.logic

import impl.runtime.Error

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol.PredicateSymbol, terms: List[Term]) {
  /**
   * Returns `true` iff the predicate is a ground truth, i.e. all terms are constant.
   */
  def isGround: Boolean = terms forall (_.isValue)

  /**
   * Optionally returns the predicate iff it is ground under the given environment `env`
   */
  def asGround(env: Map[Symbol.VariableSymbol, Value]): Option[Predicate] = {
    val terms2 = terms.map(_.asValue(env)).foldLeft(Option(List.empty[Term])) {
      case (Some(ys), Some(v)) => Some(v.asTerm :: ys)
      case _ => None
    }

    terms2 map (x => Predicate(name, x))
  }

  /**
   * Returns the predicate as ground under the given environment `env`.
   *
   * Throws an exception if the predicate is not ground.
   */
  def toGround(env: Map[Symbol.VariableSymbol, Value]): Predicate =
    asGround(env).getOrElse(throw Error.NonGroundPredicate(name))

  /**
   * Returns all (free) variable symbols in the predicate.
   */
  def variables: Set[Symbol.VariableSymbol] = terms.flatMap(_.freeVariables).toSet
}
