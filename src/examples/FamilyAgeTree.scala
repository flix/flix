package examples

import impl.logic._
import impl.runtime.Compiler
import syntax.Symbols._
import util.output.StringFormat

object FamilyAgeTree {
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

      HornClause(Predicate("AS", List(Term.Constant(Value.String("Bjarke")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(1))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Magnus")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(2))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Frits")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(3))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Orla")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Male"), Value.Int(4))))), Set.empty),

      HornClause(Predicate("AS", List(Term.Constant(Value.String("Caroline")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(5))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Inger M")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(6))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Inger T")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(7))))), Set.empty),
      HornClause(Predicate("AS", List(Term.Constant(Value.String("Grete")), Term.Constant(Value.Constructor2("AgeAndSex", Value.Constructor0("Female"), Value.Int(8))))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate("MaleGrandParent", List(Term.Variable("x"), Term.Variable("z"))), Set(
        Predicate("Parent", List(Term.Variable("x"), Term.Variable("y"))),
        Predicate("Parent", List(Term.Variable("y"), Term.Variable("z"))),
        Predicate("AS", List(Term.Variable("z"), Term.Constructor2("AgeAndSex", Term.Constructor0("Male"), Term.Variable("_"))))
      ))
    )

    val interpretations = Map(
      "Parent".asP -> Interpretation.Relation.In2(Type.String, Type.String),
      "AS".asP -> Interpretation.Relation.In2(Type.String, Type.Constructor2("AgeAndSex", Type.String, Type.Integer)),
      "MaleGrandParent".asP -> Interpretation.Relation.In2(Type.String, Type.String)
    )

    val program = Program(facts ++ clauses, interpretations)
    println(StringFormat.format(program))


    val compiler = new Compiler(program)
    compiler.verify()

    val solver = compiler.getSolver
    solver.solve()

    StringFormat.printSolution(solver)
  }
}
