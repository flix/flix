package impl

import impl.logic.PredicateSymbol

/**
 * A program consists of a set of horn clauses.
 */
case class Program(clauses: Set[HornClause], interpretation: Map[PredicateSymbol, Interpretation]) {
  /**
   * Returns the set of facts, i.e. horn clauses with an empty body.
   */
  def facts: Set[HornClause] = clauses filter (c => c.isFact)

  /**
   * Returns the set of rules, i.e. horn clauses with a non-empty body.
   */
  def rules: Set[HornClause] = clauses filterNot (c => c.isFact)
}
