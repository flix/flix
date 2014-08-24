package examples

import impl.Runner
import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object FamilyTree01 {

  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val MaleSymbol = Symbol.PredicateSymbol("Male")
    val FemaleSymbol = Symbol.PredicateSymbol("Female")
    val MaleGrandParentSymbol = Symbol.PredicateSymbol("MaleGrandParent")

    val facts = List(
      HornClause(Predicate(ParentSymbol, List(Term.Str("Caroline"), Term.Str("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Caroline"), Term.Str("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.Str("Bjarke"), Term.Str("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Bjarke"), Term.Str("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.Str("Magnus"), Term.Str("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Magnus"), Term.Str("Frits")))),


      HornClause(Predicate(ParentSymbol, List(Term.Str("Frits"), Term.Str("Inger T")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Frits"), Term.Str("Orla")))),

      HornClause(Predicate(ParentSymbol, List(Term.Str("Inger M"), Term.Str("Grete")))),

      HornClause(Predicate(MaleSymbol, List(Term.Str("Bjarke")))),
      HornClause(Predicate(MaleSymbol, List(Term.Str("Magnus")))),
      HornClause(Predicate(MaleSymbol, List(Term.Str("Frits")))),
      HornClause(Predicate(MaleSymbol, List(Term.Str("Orla")))),

      HornClause(Predicate(FemaleSymbol, List(Term.Str("Caroline")))),
      HornClause(Predicate(FemaleSymbol, List(Term.Str("Inger M")))),
      HornClause(Predicate(FemaleSymbol, List(Term.Str("Inger T")))),
      HornClause(Predicate(FemaleSymbol, List(Term.Str("Grete"))))
    )

    val clauses = List(
      HornClause(Predicate(MaleGrandParentSymbol, List(Term.Variable("x"), Term.Variable("z"))), List(
        Predicate(ParentSymbol, List(Term.Variable("x"), Term.Variable("y"))),
        Predicate(ParentSymbol, List(Term.Variable("y"), Term.Variable("z"))),
        Predicate(MaleSymbol, List(Term.Variable("z")))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation,
      MaleSymbol -> Interpretation.Relation,
      FemaleSymbol -> Interpretation.Relation,
      MaleGrandParentSymbol -> Interpretation.Relation
    )

    val hints = Map(
      ParentSymbol -> Hint(Representation.Data),
      MaleSymbol -> Hint(Representation.Data),
      FemaleSymbol -> Hint(Representation.Data),
      MaleGrandParentSymbol -> Hint(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }

}
