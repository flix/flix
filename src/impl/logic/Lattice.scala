package impl.logic

/**
 * A join semi-lattice consists of a set of elements, a bottom element, a partial ordering and a least upper bound.
 */
case class Lattice(name: Symbol.LatticeSymbol,
                   domain: Type,
                   bot: Value,
                   leq: Symbol.PredicateSymbol,
                   lub: Symbol.PredicateSymbol,
                   height: Symbol.PredicateSymbol,
                   clauses: List[HornClause])

// TODO: Need transfer function symbols?