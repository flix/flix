package examples

import impl.Runner
import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object FamilyTree02 {

  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val AgeAndSexSymbol = Symbol.PredicateSymbol("AS")
    val MaleGrandParent = Symbol.PredicateSymbol("MaleGrandParent")

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

      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Bjarke"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(1))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Magnus"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(2))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Frits"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(3))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Orla"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Int(4))))),

      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Caroline"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(5))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Inger M"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(6))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Inger T"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(7))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Str("Grete"), Term.Constructor2("AgeAndSex", Term.Constructor0("Female"), Term.Int(8)))))
    )

    val clauses = List(
      HornClause(Predicate(MaleGrandParent, List(Term.Variable("x"), Term.Variable("z"))), List(
        Predicate(ParentSymbol, List(Term.Variable("x"), Term.Variable("y"))),
        Predicate(ParentSymbol, List(Term.Variable("y"), Term.Variable("z"))),
        Predicate(AgeAndSexSymbol, List(Term.Variable("z"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Variable("_"))))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation,
      AgeAndSexSymbol -> Interpretation.Relation,
      MaleGrandParent -> Interpretation.Relation
    )

    val hints = Map(
      ParentSymbol -> Hint(Representation.Data),
      AgeAndSexSymbol -> Hint(Representation.Data),
      MaleGrandParent -> Hint(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }

}
