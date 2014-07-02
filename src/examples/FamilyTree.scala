package examples

import impl.logic._
import impl.runtime.Verifier
import syntax.Symbols._
import util.output.StringFormat

object FamilyTree {
  def main(args: Array[String]): Unit = {

    val facts = Set(
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.String("Frits")))), Set.empty),

      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.String("Frits")))), Set.empty),


      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Inger T")))), Set.empty),
      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Frits")), Term.Constant(Value.String("Orla")))), Set.empty),

      HornClause(Predicate("Parent", List(Term.Constant(Value.String("Inger M")), Term.Constant(Value.String("Grete")))), Set.empty),

      HornClause(Predicate("Male", List(Term.Constant(Value.String("Bjarke")))), Set.empty),
      HornClause(Predicate("Male", List(Term.Constant(Value.String("Magnus")))), Set.empty),
      HornClause(Predicate("Male", List(Term.Constant(Value.String("Frits")))), Set.empty),
      HornClause(Predicate("Male", List(Term.Constant(Value.String("Orla")))), Set.empty),

      HornClause(Predicate("Female", List(Term.Constant(Value.String("Caroline")))), Set.empty),
      HornClause(Predicate("Female", List(Term.Constant(Value.String("Inger M")))), Set.empty),
      HornClause(Predicate("Female", List(Term.Constant(Value.String("Inger T")))), Set.empty),
      HornClause(Predicate("Female", List(Term.Constant(Value.String("Grete")))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate("MaleGrandParent", List(Term.Variable("x"), Term.Variable("z"))), Set(
        Predicate("Parent", List(Term.Variable("x"), Term.Variable("y"))),
        Predicate("Parent", List(Term.Variable("y"), Term.Variable("z"))),
        Predicate("Male", List(Term.Variable("z")))
      ))
    )

    val interpretations = Map(
      "Parent".asP -> Interpretation.Relation.In2(Type.String, Type.String),
      "Male".asP -> Interpretation.Relation.In1(Type.String),
      "Female".asP -> Interpretation.Relation.In1(Type.String),
      "MaleGrandParent".asP -> Interpretation.Relation.In2(Type.String, Type.String)
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
