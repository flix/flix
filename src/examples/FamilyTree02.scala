package examples

import impl.logic._
import impl.runtime.Runner
import syntax.Symbols._

object FamilyTree02 {

  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val AgeAndSexSymbol = Symbol.PredicateSymbol("AS")
    val MaleGrandParent = Symbol.PredicateSymbol("MaleGrandParent")

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

      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Bjarke"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(1))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Magnus"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(2))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Frits"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(3))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Orla"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(4))))),

      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Caroline"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(5))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Inger M"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(6))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Inger T"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(7))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.String("Grete"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(8)))))
    )

    val clauses = List(
      HornClause(Predicate(MaleGrandParent, List(Term.Variable("x"), Term.Variable("z"))), List(
        Predicate(ParentSymbol, List(Term.Variable("x"), Term.Variable("y"))),
        Predicate(ParentSymbol, List(Term.Variable("y"), Term.Variable("z"))),
        Predicate(AgeAndSexSymbol, List(Term.Variable("z"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Variable("_"))))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation(Representation.Data),
      AgeAndSexSymbol -> Interpretation.Relation(Representation.Data),
      MaleGrandParent -> Interpretation.Relation(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations)

    Runner.run(program)
  }

}
