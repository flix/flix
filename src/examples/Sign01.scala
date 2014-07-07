package examples

import domains.Sign
import impl.logic._
import impl.runtime.Verifier
import util.output.StringFormat
import syntax.Symbols._

object Sign01 {

  def main(args: Array[String]): Unit = {
    val facts = Set(
      HornClause(Predicate("X", List(Term.Constant(Value.Constructor0("Pos")))), Set.empty),
      HornClause(Predicate("Y", List(Term.Constant(Value.Constructor0("Neg")))), Set.empty)
    )

    val clauses = Set(
      HornClause(Predicate("R1", List(Term.Variable("x"))), Set(Predicate("X", List(Term.Variable("x"))))),
      HornClause(Predicate("R1", List(Term.Variable("x"))), Set(Predicate("Y", List(Term.Variable("x")))))
    )

    val interpretations = Map(
      "X".asP -> Interpretation.Relation.In1,
      "Y".asP -> Interpretation.Relation.In1,
      "R1".asP -> Interpretation.Map.Leq1(Sign.lattice)
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
