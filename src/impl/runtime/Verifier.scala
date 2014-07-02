package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._

/**
 * A verifier / type checker.
 */
class Verifier(program: Program) {

  /**
   * Returns a solver for the program.
   */
  def getSolver: Solver = new Solver(program)

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {
    /**
     * Every fact must be ground (i.e. every horn clause without a body must have only values in the head predicate.)
     */
    for (f <- program.facts) {
      if (!f.isGround) {
        throw new Error.UnsafeGroundFact(f.head)
      }
    }

    /**
     * Every variable which occurs in the head of a rule must also occur in its body.
     */
    for (h <- program.clauses) {
      for (t <- h.head.terms) {
        for (v <- t.variables) {
          val found = h.body.exists(_.terms.exists(_.variables.contains(v)))
          if (!found) {
            throw new Error.UnsafeVariableSymbol(h, v)
          }
        }
      }
    }

    /**
     * Every predicate must have en interpretation.
     */
    for (p <- program.predicates) {
      program.interpretation.getOrElse(p, throw Error.InterpretationNotFound(p))
    }

    /**
     * Every head predicate must be relational.
     */


  }


  // TODO: Prove by veryfing that the negation of the properties is unsatisfiable.
  // TODO: Extract Leq symbol from the clause.
  // TODO: Need more than horn clauses???
  // TODO: Flatten?
  // TODO: Type binding for verification.

  /**
   * Function: ∀x, y. x = y => f(x) = f(y).
   */
  def function(env: Map[VSym, Type]): Formula = ???

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(env: Map[VSym, Type]): Set[HornClause] = ???


  /**
   * Anti-Symmetry: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y.
   */
  def antiSymmetry(lattice: Type.Lattice): Set[HornClause] = ???


  /**
   * Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
   */
  def transitivity(lattice: Type.Lattice): Set[HornClause] = ???

  /**
   * ∀x. ⊥ ⊑ x.
   */
  trait BottomIsLeast

  /**
   * ⨆ is total
   */
  trait JoinTotal

  /**
   * Join is Join
   * x⨆y⊑x  and x⨆y⊑y
   * ∀z:x⊑z∧y⊑z⇒x⨆y=z
   */
  trait JoinIsJoin

  // TODO: Function properties

  // If e_1⊑e_2 and e_3⊑e_4 then e_1+e_3⊑e_2+e_4
  trait Monotone

  // TODO
  trait Distributive

  // TODO
  trait Strictness

  // TODO???
  trait NoAscendingChains


}


