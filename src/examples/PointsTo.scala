package examples

import impl.logic._
import syntax.Symbols._

object PointsTo {

  val facts = Set(

  )

  val clauses = Set(
    HornClause(Predicate("VarPointsTo", List(Term.Variable("var"), Term.Variable("obj"))), Set(
      Predicate("New", List(Term.Variable("var"), Term.Variable("obj")))
    )),

    HornClause(Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("value"))), Set(
      Predicate("Assign", List(Term.Variable("var1"), Term.Variable("var2"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("value")))
    )),

    HornClause(Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("value"))), Set(
      Predicate("Load", List(Term.Variable("var1"), Term.Variable("var2"), Term.Variable("field"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("base"))),
      Predicate("HeapPointsTo", List(Term.Variable("base"), Term.Variable("field"), Term.Variable("value")))
    )),

    HornClause(Predicate("HeapPointsTo", List(Term.Variable("base"), Term.Variable("field"), Term.Variable("value"))), Set(
      Predicate("Store", List(Term.Variable("var1"), Term.Variable("field"), Term.Variable("var2"))),
      Predicate("VarPointsTo", List(Term.Variable("var1"), Term.Variable("base"))),
      Predicate("VarPointsTo", List(Term.Variable("var2"), Term.Variable("value")))
    ))
  )

  val interpretation = Map(
    "New" -> Interpretation.Relation.In2,
    "Assign" -> Interpretation.Relation.In2,
    "Load" -> Interpretation.Relation.In3,
    "Store" -> Interpretation.Relation.In3
  )

}
