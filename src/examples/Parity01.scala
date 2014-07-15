package examples

import domains.Parity
import impl.logic._
import impl.runtime.{Hint, Representation, Runner}
import syntax.Symbols._

object Parity01 {

  def main(args: Array[String]): Unit = {

    val X = Symbol.PredicateSymbol("X")
    val Y = Symbol.PredicateSymbol("Y")
    val R = Symbol.PredicateSymbol("R")

    val facts = List(
      HornClause(Predicate(X, List(Term.Constructor0("Parity.Odd")))),
      HornClause(Predicate(Y, List(Term.Constructor0("Parity.Even"))))
    )

    val clauses = List(
      HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(X, List(Term.Variable("x"))))),
      HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(Y, List(Term.Variable("x")))))
    )

    val interpretations = Map(
      X -> Interpretation.Relation,
      Y -> Interpretation.Relation,
      R -> Interpretation.Lattice
    )

    val hints = Map(
      X -> Hint(Representation.Data),
      Y -> Hint(Representation.Data),
      R -> Hint(Representation.Data)
    ) ++ Parity.Hints

    val lattices = Map(
      R -> Parity.lattice
    )

    val program = Program(facts ::: clauses ::: Parity.lattice.clauses, interpretations, lattices)

    Runner.run(program, hints)
  }

}
