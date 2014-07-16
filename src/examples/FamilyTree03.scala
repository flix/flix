package examples

import impl.Runner
import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object FamilyTree03 {

  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val AgeSymbol = Symbol.PredicateSymbol("Age")
    val ParentChildSymbol = Symbol.PredicateSymbol("P")

    val facts = List(
      HornClause(Predicate(ParentSymbol, List(Term.String("Caroline"), Term.String("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Caroline"), Term.String("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.String("Bjarke"), Term.String("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Bjarke"), Term.String("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.String("Magnus"), Term.String("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Magnus"), Term.String("Frits")))),

      HornClause(Predicate(AgeSymbol, List(Term.String("Caroline"), Term.String("17")))),
      HornClause(Predicate(AgeSymbol, List(Term.String("Bjarke"), Term.String("23")))),
      HornClause(Predicate(AgeSymbol, List(Term.String("Magnus"), Term.String("28"))))
    )

    val clauses = List(
      HornClause(Predicate(ParentChildSymbol, List(Term.Variable("parent"), Term.Constructor2("NameAndAge", Term.Variable("child"), Term.Variable("age")))), List(
        Predicate(ParentSymbol, List(Term.Variable("child"), Term.Variable("parent"))),
        Predicate(AgeSymbol, List(Term.Variable("child"), Term.Variable("age")))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation,
      AgeSymbol -> Interpretation.Relation,
      ParentChildSymbol -> Interpretation.Relation
    )

    val hints = Map(
      ParentSymbol -> Hint(Representation.Data),
      AgeSymbol -> Hint(Representation.Data),
      ParentChildSymbol -> Hint(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }
}
