package impl.logic

sealed trait Interpretation

object Interpretation {

  /**
   * A semantics where the predicate P(x) holds iff x ∈ P.
   */
  case object Relation extends Interpretation

  /**
   * A semantics where the predicate P(x) holds iff x ⊑ P.
   */
  case object Lattice extends Interpretation

  /**
   * A semantics where the predicate P() holds iff |P| = 1.
   */
  case object Singleton extends Interpretation

  /**
   * A semantics where the predicate P() holds iff |x| > 1.
   */
  case object NonSingleton extends Interpretation

  /**
   * A semantics where the predicate P(c, x) holds iff c ⊑ x.
   */
  case object Atleast extends Interpretation

}
