package examples

import impl.logic._
import syntax.Symbols._

object PointsTo {

  val clauses = List(
    HornClause(Predicate("VarPointsTo", List(Term.Variable("var"), Term.Variable("obj"))), List(
      Predicate("New", List(Term.Variable("var"), Term.Variable("obj")))
    )),

    HornClause(Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("value"))), List(
      Predicate("Assign", List(Term.Variable("var1"), Term.Variable("var2"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("value")))
    )),

    HornClause(Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("value"))), List(
      Predicate("Load", List(Term.Variable("var1"), Term.Variable("var2"), Term.Variable("field"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("base"))),
      Predicate("HeapPointsTo", List(Term.Variable("base"), Term.Variable("field"), Term.Variable("value")))
    )),

    HornClause(Predicate("HeapPointsTo", List(Term.Variable("base"), Term.Variable("field"), Term.Variable("value"))), List(
      Predicate("Store", List(Term.Variable("var1"), Term.Variable("field"), Term.Variable("var2"))),
      Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("base"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("value")))
    ))
  )

}
