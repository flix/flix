package examples

import impl.logic._
import impl.runtime.Verifier
import syntax.Symbols._
import util.output.StringFormat

object FamilyTree01 {
  def main(args: Array[String]): Unit = {

    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val MaleSymbol = Symbol.PredicateSymbol("Male")
    val FemaleSymbol = Symbol.PredicateSymbol("Female")
    val MaleGrandParentSymbol = Symbol.PredicateSymbol("MaleGrandParent")

    val facts = Set(
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Frits")))), Set.empty),


      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Inger T")))), Set.empty),
      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Orla")))), Set.empty),

      HornClause(Predicate(ParentSymbol, List(Term.Constant(Value.String("Inger M")), Term.Constant(Value.String("Grete")))), Set.empty),

      HornClause(Predicate(MaleSymbol, List(Term.Constant(Value.String("Bjarke")))), Set.empty),
      HornClause(Predicate(MaleSymbol, List(Term.Constant(Value.String("Magnus")))), Set.empty),
      HornClause(Predicate(MaleSymbol, List(Term.Constant(Value.String("Frits")))), Set.empty),
      HornClause(Predicate(MaleSymbol, List(Term.Constant(Value.String("Orla")))), Set.empty),

      HornClause(Predicate(FemaleSymbol, List(Term.Constant(Value.String("Caroline")))), Set.empty),
      HornClause(Predicate(FemaleSymbol, List(Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate(FemaleSymbol, List(Term.Constant(Value.String("Inger T")))), Set.empty),
      HornClause(Predicate(FemaleSymbol, List(Term.Constant(Value.String("Grete")))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate(MaleGrandParentSymbol, List(Term.Variable("x"), Term.Variable("z"))), Set(
        Predicate(ParentSymbol, List(Term.Variable("x"), Term.Variable("y"))),
        Predicate(ParentSymbol, List(Term.Variable("y"), Term.Variable("z"))),
        Predicate(MaleSymbol, List(Term.Variable("z")))
      ))
    )

    val interpretations = Map(
      ParentSymbol -> Interpretation.Relation(Representation.Data),
      MaleSymbol -> Interpretation.Relation(Representation.Data),
      FemaleSymbol -> Interpretation.Relation(Representation.Data),
      MaleGrandParentSymbol -> Interpretation.Relation(Representation.Data)
    )

    val program = Program(facts ++ clauses, interpretations)

    val compiler = new Verifier(program)
    compiler.verify()

    val solver = compiler.getSolver
    solver.solve()

    println(StringFormat.format(program))
    StringFormat.printSolution(solver)
  }
}
