package examples

import impl.logic._
import impl.runtime.{Hint, Representation, Runner}
import syntax.Symbols._

object FamilyTree01 {

  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val MaleSymbol = Symbol.PredicateSymbol("Male")
    val FemaleSymbol = Symbol.PredicateSymbol("Female")
    val MaleGrandParentSymbol = Symbol.PredicateSymbol("MaleGrandParent")

    val facts = List(
      HornClause(Predicate(ParentSymbol, List(Term.String("Caroline"), Term.String("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Caroline"), Term.String("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.String("Bjarke"), Term.String("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Bjarke"), Term.String("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.String("Magnus"), Term.String("Inger M")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Magnus"), Term.String("Frits")))),


      HornClause(Predicate(ParentSymbol, List(Term.String("Frits"), Term.String("Inger T")))),
      HornClause(Predicate(ParentSymbol, List(Term.String("Frits"), Term.String("Orla")))),

      HornClause(Predicate(ParentSymbol, List(Term.String("Inger M"), Term.String("Grete")))),

      HornClause(Predicate(MaleSymbol, List(Term.String("Bjarke")))),
      HornClause(Predicate(MaleSymbol, List(Term.String("Magnus")))),
      HornClause(Predicate(MaleSymbol, List(Term.String("Frits")))),
      HornClause(Predicate(MaleSymbol, List(Term.String("Orla")))),

      HornClause(Predicate(FemaleSymbol, List(Term.String("Caroline")))),
      HornClause(Predicate(FemaleSymbol, List(Term.String("Inger M")))),
      HornClause(Predicate(FemaleSymbol, List(Term.String("Inger T")))),
      HornClause(Predicate(FemaleSymbol, List(Term.String("Grete"))))
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
