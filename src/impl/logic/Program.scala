package impl.logic

// TODO:
// DONE 00. Introduce Unit.
// DONE 01. Introduce proper variants.
// DONE 02. Introduce proper tuples.
// 02. Introduce let binding
// 03. Introduce bool, int and string operators.
// 04. Introduce set and its operators.
// 05. Ensure that the lambda evaluator works together with substitution...
// 06. Introduce proper types for all of the above.
// 07. Annotate lamdas with types.
// 08. Write type checking algorithm.
// 09. Introduce pattern datatype and pattern match term.
// 10. Introduce types in horn clauses.
// 11. Change definition of a program to be a list of rules and a list of named lambdas, and a list of declared types.
// 12. Rewrite solver to use lambdas and types.
// 13. Remove deprecated code.
// 14. Introduce s-expression like language for all of the above.
// 15. Desugar Terms and Types to SMT Lib

/**
 * A program consists of a set of horn clauses.
 */
case class Program(clauses: List[HornClause], @deprecated interpretation: Map[Symbol.PredicateSymbol, Interpretation], @deprecated lattices: Map[Symbol.PredicateSymbol, Lattice]) {
  /**
   * Returns the set of facts, i.e. horn clauses with an empty body.
   */
  def facts: List[HornClause] = clauses filter (c => c.isFact)

  /**
   * Returns the set of rules, i.e. horn clauses with a non-empty body.
   */
  def rules: List[HornClause] = clauses filterNot (c => c.isFact)

  /**
   * Returns all predicate symbols in all horn clauses.
   */
  def predicates: Set[Symbol.PredicateSymbol] = (clauses flatMap (_.predicates)).toSet

}
