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
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Inger M"))))),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Frits"))))),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Inger M"))))),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Frits"))))),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Inger M"))))),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Frits"))))),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Inger T"))))),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Orla"))))),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Inger M")), Term.Constant(Value.String("Grete"))))),

      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(1)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(2)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Frits")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(3)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Orla")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(4)))))),

      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(5)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Inger M")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(6)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Inger T")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(7)))))),
      HornClause(Predicate(AgeAndSexSymbol, List(Term.Constant(Value.String("Grete")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(8))))))
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
