package impl.logic

/**
 * TODO: Doc
 */
// TODO: Cleanup
case class Lattice(elms: Type, bot: Value, leq: Set[HornClause], leqSymbol: Symbol.PredicateSymbol, join: Set[HornClause])