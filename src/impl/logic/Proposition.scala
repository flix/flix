package impl.logic

/**
 * A propositional logic formula with the theory of equality.
 */
sealed trait Proposition {

  /**
   * Returns `true` iff the proposition (and its subterms) contain
   * at least one free variable under the given environment `env`.
   */
  def isOpen(env: Map[Symbol.VariableSymbol, Value]): Boolean = !isClosed(env)

  /**
   * Returns `true` iff the proposition (and its subterms) contain
   * no free variables under the given environment `env`.
   */
  def isClosed(env: Map[Symbol.VariableSymbol, Value]): Boolean =
    freeVars subsetOf env.keySet

  /**
   * Returns the set of free variables in the proposition.
   */
  def freeVars: Set[Symbol.VariableSymbol] = this match {
    case Proposition.Not(p) => p.freeVars
    case Proposition.Conj(ps) => ps.flatMap(_.freeVars).toSet
    case Proposition.Disj(ps) => ps.flatMap(_.freeVars).toSet
    case Proposition.Eq(x, y) => x.freeVariables ++ y.freeVariables
    case Proposition.NotEq(x, y) => x.freeVariables ++ y.freeVariables
  }

}

object Proposition {

  /**
   * The negation of the proposition `p`.
   */
  case class Not(p: Proposition) extends Proposition

  /**
   * The conjunction of the propositions `ps`.
   */
  case class Conj(ps: List[Proposition]) extends Proposition

  /**
   * The disjunction of the propositions `ps`.
   */
  case class Disj(ps: List[Proposition]) extends Proposition

  /**
   * The equality of the two terms `x` and `y`.
   */
  case class Eq(x: Term, y: Term) extends Proposition

  /**
   * The in-equality of the two terms `x` and `y`.
   */
  case class NotEq(x: Term, y: Term) extends Proposition

}
