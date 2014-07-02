package impl.runtime

import impl.logic._

/**
 * A verifier / type checker.
 */
class Verifier(program: Program) {

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {
    // 1. Each fact must be ground.
    program.facts.find(!_.isGround).exists(f => throw new RuntimeException(s"The fact $f is not ground!"))

    // 2. Each variable which occurs in the head of a rule of must also occur in its body.
  }

  /**
   * Returns a solver for the program.
   */
  def getSolver: Solver = new Solver(program)

  
  
  
  
  // TODO: Prove by veryfing that the negation of the properties is unsatisfiable.
  // TODO: Extract Leq symbol from the clause.
  // TODO: Need more than horn clauses???
  // TODO: Flatten?
  // TODO: Type binding for verification.

  /**
   * Function: ∀x, y. x = y => f(x) = f(y).
   */
  def function(env: Map[Symbol, Type]): Formula = ???

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(lattice: Type.Lattice): Set[HornClause] = ???

  //    lattice.elms match {
  //    case Type.Nominal(symbol) =>
  //      // Leq(symbol, symbol).
  //      Set(
  //        HornClause(Predicate("Leq".asP, List(Term.Constant(Value.Constructor0(symbol)), Term.Constant(Value.Constructor0(symbol)))), Set.empty)
  //      )
  //    case Type.Boolean =>
  //      // Leq(true, true).
  //      // Leq(false, false).
  //      Set(
  //        HornClause(Predicate("Leq".asP, List(Term.Constant(Value.Bool(b = true)), Term.Constant(Value.Bool(b = true)))), Set.empty),
  //        HornClause(Predicate("Leq".asP, List(Term.Constant(Value.Bool(b = false)), Term.Constant(Value.Bool(b = false)))), Set.empty)
  //      )
  //    case Type.Integer =>
  //      // Leq(x, x).
  //      // Int(x).
  //      Set(
  //        HornClause(Predicate("Int".asP, List(Term.Variable('x))), Set.empty),
  //        HornClause(Predicate("Leq".asP, List(Term.Variable('x))), Set.empty)
  //      )
  //    case _ => ???
  //  }

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

  def fresh(t: Type): Term.Variable = ???

  def leq(e1: Term.Variable, t1: Type, e2: Term.Variable, t2: Type): Set[HornClause] = ???

}


