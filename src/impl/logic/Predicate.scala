package impl.logic

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol.PredicateSymbol, terms: List[Term], typ: Type)

// TODO: Consider splitting into two one for terms and one for values.