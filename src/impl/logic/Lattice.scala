package impl.logic

/**
 * TODO: Doc
 */
case class Lattice(elms: Type, bot: Value, leq: Set[HornClause], join: Set[HornClause])