package impl.logic

/**
 * A horn clause consists of a single predicate called the `head` and a set of predicates called the `body`.
 */
case class HornClause(head: Predicate, body: List[Predicate] = List.empty) {
  /**
   * Returns `true` if the clause is a fact, i.e. if the body is empty.
   */
  def isFact: Boolean = body.isEmpty

  /**
   * Returns `true` if the clause is a ground truth, i.e. all predicates (head and body) are ground truths.
   */
  def isGround: Boolean = head.isGround && (body forall (_.isGround))
}
