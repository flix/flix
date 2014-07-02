package examples

import impl.logic._
import impl.runtime.Verifier
import syntax.Symbols._
import util.output.StringFormat

object FamilyTree03 {
  def main(args: Array[String]): Unit = {

    val facts = Set(
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Inger")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate("Age", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("17")))), Set.empty),
      HornClause(Predicate("Age", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("23")))), Set.empty),
      HornClause(Predicate("Age", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("28")))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate("P", List(Term.Variable("parent"), Term.Constructor2("NameAndAge", Term.Variable("child"), Term.Variable("age")))), Set(
        Predicate("Parent", List(Term.Variable("child"), Term.Variable("parent"))),
        Predicate("Age", List(Term.Variable("child"), Term.Variable("age")))
      ))
    )

    val interpretations = Map(
      "Parent".asP -> Interpretation.Relation.In2,
      "Age".asP -> Interpretation.Relation.In2,
      "P".asP -> Interpretation.Relation.In2
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
