package examples

import impl.Runner
import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object Graph01 {

  def main(args: Array[String]): Unit = {

    val EdgeSymbol = Symbol.PredicateSymbol("Edge")
    val CycleSymbol = Symbol.PredicateSymbol("Cycle")

    val facts = List(
      HornClause(Predicate(EdgeSymbol, List(Term.String("c"), Term.String("d")))),
      HornClause(Predicate(EdgeSymbol, List(Term.String("b"), Term.String("c")))),
      HornClause(Predicate(EdgeSymbol, List(Term.String("a"), Term.String("b")))),
      HornClause(Predicate(EdgeSymbol, List(Term.String("d"), Term.String("a"))))
    )

    val clauses = List(
      // E(x, z) :- E(x, y), E(y, z).
      HornClause(head =
        Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("z"))),
        body = List(
          Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("y"))),
          Predicate(EdgeSymbol, List(Term.Variable("y"), Term.Variable("z")))
        )),

      // C(x) :- E(x, x).
      HornClause(head =
        Predicate(CycleSymbol, List(Term.Variable("x"))),
        body = List(
          Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("x")))
        ))
    )

    val interpretations = Map(
      EdgeSymbol -> Interpretation.Relation,
      CycleSymbol -> Interpretation.Relation
    )

    val hints = Map(
      EdgeSymbol -> Hint(Representation.Data),
      CycleSymbol -> Hint(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }


}
