package impl

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol, terms: List[Term]) {
  /**
   * Returns `true` iff the predicate is a ground truth, i.e. all terms are constant.
   */
  def isGround: Boolean = terms forall (_.isConstant)

  /**
   * Returns `true` iff the predicate is nullary, i.e. a proposition without any terms.
   */
  def isNullary: Boolean = terms.isEmpty
}
