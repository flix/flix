package impl.runtime

import impl.ast.SExp
import impl.logic._
import syntax.Constraints.RichConstraint
import syntax.Predicates.RichPredicate
import syntax.Propositions.RichProposition
import syntax.Types.RichType
import syntax.Terms.RichTerm
import syntax.Symbols.RichSymbol
import syntax.Values.RichValue

object Error {

  /**
   * An exception thrown to indicate that the variable `x` was expected to be bound, but is actually unbound.
   */
  case class UnboundVariableError(x: Symbol.VariableSymbol)
    extends RuntimeException(s"Expected the variable ${x.fmt} to be bound.")

  /**
   * An exception thrown to indicate a run-time type error.
   */
  case class RuntimeTypeError(expected: Type, actual: Value)
    extends RuntimeException(s"Expected a value of type ${expected.fmt} but got ${actual.fmt}.")

  /**
   * An exception thrown to indicate a static type error in a term.
   */
  case class TermTypeError(types: List[Type], t: Term)
    extends RuntimeException(s"Unification Error: ${types.map(_.fmt).mkString(", ")} in term ${t.fmt}.")

  /**
   * An exception thrown to indicate a static type error in a predicate.
   */
  case class PredicateTypeError(types: List[Type], p: Predicate, c: Constraint)
    extends RuntimeException(s"Unification Error: ${types.map(_.fmt).mkString(", ")} in predicate ${p.fmt} (inside constraint: ${c.fmt}).")

  /**
   * An exception thrown to indicate a static type error in a proposition.
   */
  case class PropositionTypeError(types: List[Type], p: Proposition)
    extends RuntimeException(s"Unification Error: ${types.map(_.fmt).mkString(", ")} in proposition ${p.fmt}.")

  /**
   * An exception thrown to indicate that the bottom value for `typ` is missing.
   */
  case class MissingBot(typ: Type)
    extends RuntimeException(s"Missing bottom value for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the less-than-equal function for `typ` is missing.
   */
  case class MissingLeq(typ: Type)
    extends RuntimeException(s"Missing less-than-equal for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the least-upper-bound function for `typ` is missing.
   */
  case class MissingLub(typ: Type)
    extends RuntimeException(s"Missing least-upper-bound for type ${typ.fmt}")

  /**
   * An exception thrown to indicate that the height function for `typ` is missing.
   */
  case class MissingHeight(typ: Type)
    extends RuntimeException(s"Missing height function for type ${typ.fmt}")

  /**
   * An exception thrown to indicate a parsing error.
   */
  case class ParseError(e: SExp)
    extends RuntimeException(s"Unable to parse $e")

}
