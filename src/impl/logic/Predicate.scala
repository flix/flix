package impl.logic

import impl.runtime.Error

/**
 * A predicate consists of a predicate symbol and a list of terms.
 */
case class Predicate(name: Symbol.PredicateSymbol, terms: List[Term], typ: Type) {
  /**
   * Optionally returns the predicate iff it is ground under the given environment `env`
   */
  def asGround(env: Map[Symbol.VariableSymbol, Value]): Option[Predicate] = {
//    val terms2 = terms.map(_.asValue(env)).foldLeft(Option(List.empty[Term])) {
//      case (Some(ys), Some(v)) => Some(v.toTerm :: ys)
//      case _ => None
//    }
//
//    terms2 map (x => Predicate(name, x))
    ???
  }

  /**
   * Returns the predicate as ground under the given environment `env`.
   *
   * Throws an exception if the predicate is not ground.
   */
  @deprecated("", "")
  def toGround(env: Map[Symbol.VariableSymbol, Value]): Predicate =
    asGround(env).getOrElse(throw Error.NonGroundPredicate(name))
}
