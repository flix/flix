package examples

import domains.Sign
import impl.logic._
import impl.runtime.Runner
import syntax.Symbols._

object SignAndParity01 {

  def main(args: Array[String]): Unit = {

    val X = Symbol.PredicateSymbol("X")
    val Y = Symbol.PredicateSymbol("Y")
    val R = Symbol.PredicateSymbol("R")

    val facts = List(
      HornClause(Predicate(X, List(Term.Constructor2("SP", Term.Constant(Value.Constructor0("Pos")), Term.Constant(Value.Constructor0("Odd")))))),
      HornClause(Predicate(Y, List(Term.Constructor2("SP", Term.Constant(Value.Constructor0("Bot")), Term.Constant(Value.Constructor0("Even"))))))
    )

    val clauses = List(
      HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(X, List(Term.Variable("x"))))),
      HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(Y, List(Term.Variable("x")))))
    )

    val interpretations = Map(
      X -> Interpretation.Relation(Representation.Data),
      Y -> Interpretation.Relation(Representation.Data),
      R -> Interpretation.LatticeMap(Sign.lattice)
    ) ++ Sign.lattice.interpretation

    val program = Program(facts ::: clauses ::: Sign.lattice.clauses, interpretations)

    Runner.run(program)
  }

}
