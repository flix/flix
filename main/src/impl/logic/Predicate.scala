package impl.logic


/**
 * A predicate is either a ground predicate or a non-ground predicate.
 */
sealed trait Predicate {
  def name: Symbol.PredicateSymbol
  def terms: List[Term]
  def typ: Type
}

object Predicate {

  /**
   * A ground predicate is a predicate where all terms are values.
   */
  case class GroundPredicate(name: Symbol.PredicateSymbol, values: List[Value], typ: Type) extends Predicate {
    def terms: List[Term] = values map (_.toTerm)
  }

  /**
   * A non-ground predicate is a predicate where not all terms are values.
   */
  case class NonGroundPredicate(name: Symbol.PredicateSymbol, terms: List[Term], typ: Type) extends Predicate

}