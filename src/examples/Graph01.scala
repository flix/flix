package examples

import impl.logic._
import impl.runtime.Runner
import syntax.Symbols._

object Graph01 {

  def main(args: Array[String]): Unit = {

    val EdgeSymbol = Symbol.PredicateSymbol("Edge")
    val CycleSymbol = Symbol.PredicateSymbol("Cycle")

    val facts = List(
      HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.String("a")), Term.Constant(Value.String("b"))))),
      HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.String("a")), Term.Constant(Value.String("a"))))),
      HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.String("b")), Term.Constant(Value.String("c"))))),
      HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.String("c")), Term.Constant(Value.String("d"))))),
      HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.String("d")), Term.Constant(Value.String("a")))))
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
      EdgeSymbol -> Interpretation.Relation(Representation.Data),
      CycleSymbol -> Interpretation.Relation(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations)

    Runner.run(program)
  }


}
