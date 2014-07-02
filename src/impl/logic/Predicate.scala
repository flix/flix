package impl.logic

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol.PredicateSymbol, terms: List[Term]) {
  /**
   * Returns `true` iff the predicate is a ground truth, i.e. all terms are constant.
   */
  def isGround: Boolean = terms forall (_.isValue)
}
