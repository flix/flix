package impl.logic

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol.PredicateSymbol, terms: List[Term]) {
  /**
   * Returns `true` iff the predicate is a ground truth, i.e. all terms are constant.
   */
  def isGround: Boolean = terms forall (_.isValue)

  /**
   * Returns all (free) variable symbols in the predicate.
   */
  def variables: Set[Symbol.VariableSymbol] = terms.flatMap(_.variables).toSet
}
