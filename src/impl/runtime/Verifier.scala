package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym, VariableSymbol}
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
      for (v <- h.variables) {
        val found = h.body.exists(c => c.variables.contains(v))
        if (!found) {
          throw new Error.UnsafeVariableSymbol(h, v)
        }
      }
    }

    /**
     * Every predicate must have en interpretation.
     */
    for (p <- program.predicates) {
      program.interpretation.get(p) match {
        case None => throw Error.InterpretationNotFound(p)
        case Some(i) => // nop - interpretation exists.
      }
    }

    /**
     * Every head predicate must be relational.
     */
    for (h <- program.clauses) {
      val i = program.interpretation(h.head.name)
      if (!i.isRelational) {
        throw new Error.NonRelationalPredicate(h.head.name)
      }
    }
  }


  /**
   * Function: ∀x, y. x = y => f(x) = f(y).
   */
  def function(f: PSym): Formula = typer(
    Formula.Implication(
      Formula.Atom(Predicate(Symbol.PredicateSymbol("Eq"), List(
        Term.Variable(Symbol.VariableSymbol("x")),
        Term.Variable(Symbol.VariableSymbol("y"))
      ))),

    // TODO: Need some notion of equality??
      Formula.Conjunction(Set(
        Formula.Atom(Predicate(Symbol.PredicateSymbol("Eq"), List(
          Term.Variable(Symbol.VariableSymbol("fx")),
          Term.Variable(Symbol.VariableSymbol("fy"))
        )))
      ))
    )
  )

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(lattice: Lattice, tenv: Map[Symbol, Type]): Formula = typer(
    Formula.Atom(Predicate(lattice.leqSymbol, List(Term.Variable(Symbol.VariableSymbol("x")), Term.Variable(Symbol.VariableSymbol("x")))))
  )


  /**
   * Anti-Symmetry: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y.
   */
  def antiSymmetry(lattice: Lattice): Formula = typer(
    Formula.Implication(
      Formula.Conjunction(Set(
        Formula.Atom(Predicate(lattice.leqSymbol, List(Term.Variable(Symbol.VariableSymbol("x")), Term.Variable(Symbol.VariableSymbol("y"))))),
        Formula.Atom(Predicate(lattice.leqSymbol, List(Term.Variable(Symbol.VariableSymbol("y")), Term.Variable(Symbol.VariableSymbol("x")))))
      )
      ),
      Formula.Atom(Predicate(Symbol.PredicateSymbol("Eq"), List(Term.Variable(Symbol.VariableSymbol("y")), Term.Variable(Symbol.VariableSymbol("x"))))
      )
    ))


  /**
   * Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
   */
  def transitivity(): Formula = ???

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

  /////////////////////////////////////////////////////////////////////////////
  // Typer                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  def typer(f: Formula): Formula = ???

}


