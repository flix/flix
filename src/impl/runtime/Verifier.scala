package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import syntax.Symbols._

/**
 * A verifier / type checker.
 */
class Verifier(val program: Program) {

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {
    /**
     * Every predicate must have en interpretation.
     */
    for (p <- program.predicates) {
      program.interpretation.get(p) match {
        case None => throw Error.InterpretationNotFound(p)
        case Some(i) => // nop - interpretation exists.
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Lattice Properties                                                      //
  /////////////////////////////////////////////////////////////////////////////

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(leq: PSym): Formula = typer(
    Formula.Forall("x", Formula.Atom(Predicate(leq, List(Term.Variable("x"), Term.Variable("x")))))
  )

  /**
   * Anti-Symmetry: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y.
   */
  def antiSymmetry(leq: PSym): Formula = typer(
    Formula.Implication(
      Formula.Conjunction(Set(
        Formula.Atom(Predicate(leq, List(Term.Variable(Symbol.VariableSymbol("x")), Term.Variable(Symbol.VariableSymbol("y"))))),
        Formula.Atom(Predicate(leq, List(Term.Variable(Symbol.VariableSymbol("y")), Term.Variable(Symbol.VariableSymbol("x")))))
      )),
      Formula.Atom(Predicate(Symbol.PredicateSymbol("Eq"), List(Term.Variable(Symbol.VariableSymbol("y")), Term.Variable(Symbol.VariableSymbol("x")))))
    )
  )

  /**
   * Transitivity: ∀x, y, z. x ⊑ y ∧ y ⊑ z ⇒ x ⊑ z.
   */
  def transitivity(leq: PSym): Formula = typer(
    Formula.Implication(
      Formula.Conjunction(Set(
        Formula.Atom(Predicate(leq, List(Term.Variable(Symbol.VariableSymbol("x")), Term.Variable(Symbol.VariableSymbol("y"))))),
        Formula.Atom(Predicate(leq, List(Term.Variable(Symbol.VariableSymbol("y")), Term.Variable(Symbol.VariableSymbol("z")))))
      )),
      Formula.Atom(Predicate(leq, List(Term.Variable(Symbol.VariableSymbol("x")), Term.Variable(Symbol.VariableSymbol("z")))))
    )
  )

  /**
   * Least Element: ∀x. ⊥ ⊑ x.
   */
  def leastElement(leq: PSym, bot: Value): Formula = typer(
    Formula.Atom(Predicate(leq, List(Term.Constant(bot), Term.Variable("x"))))
  )


  // TODO: ~~~~~~~~~~~~~Function properties~~~~~~~~~~~~

  /**
   * Function: ∀x, y. x = y ⇒ f(x) = f(y).
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
   * ⨆ is total
   */
  trait JoinTotal

  /**
   * Join is Join
   * 1. x ⊑ x ⨆ y ∧ y ⊑ x ⨆ y
   *
   * 2. ∀z, x ⊑ z ∧ y ⊑ z ⇒ x ⨆ y = z
   */
  trait JoinIsJoin


  /**
   * Monotonicity: ∀x, x ⊑ y ⇒ f(x) ⊑ f(y).
   */
  // TODO: What about n-ary predicates?
  def monotonicity(f: PSym, leq: PSym): Formula = typer(
    Formula.Implication(
      Formula.Atom(Predicate(leq, List(Term.Variable("x"), Term.Variable("y")))),
      ??? // TODO: Again we need equality
    )
  )

  /**
   * Strictness: f(⊥) = ⊥
   */
  // TODO
  trait Strictness

  /**
   * Distributivity: ∀x, y, f(x ⨆ y) = f(x) ⨆ f(y).
   */
  // TODO
  trait Distributivity


  /////////////////////////////////////////////////////////////////////////////
  // Typer                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  def typer(f: Formula): Formula = {
    val t: Type = ???
    val tenv: Map[VSym, Type] = ???

    f match {
      case Formula.Forall(s, x) => t match {
        case Type.Constructor0(ns) => typer(x)
        case _ => ???
      }
      case _ => ???
    }
  }


  def unify(t: Term, tt: Type, env: Map[Term, Type]) = ???



}
