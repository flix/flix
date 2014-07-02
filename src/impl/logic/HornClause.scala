package impl.logic

/**
 * A horn clause consists of a single predicate called the `head` and a set of predicates called the `body`.
 */
case class HornClause(head: Predicate, body: Set[Predicate]) {
  /**
   * Returns `true` if the clause is a fact, i.e. if the body is empty.
   */
  def isFact: Boolean = body.isEmpty

  /**
   * Returns `true` if the clause is a ground truth, i.e. all predicates (head and body) are ground truths.
   */
  def isGround: Boolean = head.isGround && (body forall (_.isGround))

  /**
   * Returns all predicate symbols in the head and body.
   */
  def predicates: Set[Symbol.PredicateSymbol] = Set(head.name) ++ (body map (_.name))
}
