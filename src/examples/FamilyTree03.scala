package examples

import impl.logic._
import impl.runtime.Verifier
import syntax.Symbols._
import util.output.StringFormat

object FamilyTree03 {
  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val AgeSymbol = Symbol.PredicateSymbol("Age")
    val ParentChildSymbol = Symbol.PredicateSymbol("P")

    val facts = Set(
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate(AgeSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("17")))), Set.empty),
      HornClause(Predicate(AgeSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("23")))), Set.empty),
      HornClause(Predicate(AgeSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("28")))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate(ParentChildSymbol, List(Term.Variable("parent"), Term.Constructor2("NameAndAge", Term.Variable("child"), Term.Variable("age")))), Set(
        Predicate(ParentSymbol, List(Term.Variable("child"), Term.Variable("parent"))),
        Predicate(AgeSymbol, List(Term.Variable("child"), Term.Variable("age")))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation(Representation.Data),
      AgeSymbol -> Interpretation.Relation(Representation.Data),
      ParentChildSymbol -> Interpretation.Relation(Representation.Data)
    )

    val program = Program(facts ++ clauses, interpretations)
    println(StringFormat.format(program))


    val compiler = new Verifier(program)
    compiler.verify()

    val solver = compiler.getSolver
    solver.solve()

    StringFormat.printSolution(solver)
  }
}
