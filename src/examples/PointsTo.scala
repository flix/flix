package examples

import impl.logic.Term.Variable
import impl.logic._
import impl.runtime.Solver
import syntax.Symbols._

object PointsTo {

  def main(args: Array[String]): Unit = {


    val clauses = Set(
      // Constraint VarPointsTo(var, obj) :-
      //   New(var, obj).
      HornClause(Predicate("VarPointsTo".asP, List(Variable("var"), Variable("obj"))), Set(
        Predicate("New".asP, List(Variable("var"), Variable("obj")))
      )),

      // Constraint VarPointsTo(var1, value) :-
      //   Assign(var1, var2),
      //   VarPointsTo(var2, value).
      HornClause(Predicate("VarPointsTo".asP, List(Variable("var1"), Variable("value"))), Set(
        Predicate("Assign".asP, List(Variable("var1"), Variable("var2"))),
        Predicate("VarPointsTo".asP, List(Variable("var2"), Variable("value")))
      )),

      // Constraint VarPointsTo(var1, value) :-
      //   Load(var1, var2, field),
      //   VarPointsTo(var2, base),
      //   HeapPointsTo(base, field, value).
      HornClause(Predicate("VarPointsTo".asP, List(Variable("var1"), Variable("value"))), Set(
        Predicate("Load".asP, List(Variable("var1"), Variable("var2"), Variable("field"))),
        Predicate("VarPointsTo".asP, List(Variable("var2"), Variable("base"))),
        Predicate("HeapPointsTo".asP, List(Variable("base"), Variable("field"), Variable("value")))
      )),

      // Constraint HeapPointsTo(base, field, value) :-
      //   Store(var1, field, var2),
      //   VarPointsTo(var1, base),
      //   VarPointsTo(var2, value).
      HornClause(Predicate("HeapPointsTo".asP, List(Variable("base"), Variable("field"), Variable("value"))), Set(
        Predicate("Store".asP, List(Variable("var1"), Variable("field"), Variable("var2"))),
        Predicate("VarPointsTo".asP, List(Variable("var1"), Variable("base"))),
        Predicate("VarPointsTo".asP, List(Variable("var2"), Variable("value")))
      ))
    )

    val facts = Set(
      HornClause(Predicate("New".asP, List(Term.Constant(Value.Int(0)), Term.Constant(Value.Int(1)))), Set.empty)
    )

    val VariableType = Type.Constructor0("VariableT")
    val FieldType = Type.Constructor0("FieldType")
    val ObjectType = Type.Constructor0("ObjectType")

    val SignType = Type.Variant(IndexedSeq(
      Type.Constructor0("Bot"),
      Type.Constructor0("Top"),
      Type.Constructor0("Neg"),
      Type.Constructor0("Zero"),
      Type.Constructor0("Pos")
    ))

    // Lattice
    val LatticeType = Type.Lattice(
      elms = SignType,
      bot = Value.Constructor0("Bot"),
      order = Set.empty,
      join = Set.empty
    )

    val interpretation = Map(
      "New".asP -> Interpretation.Relation.In2,
      "Assign".asP -> Interpretation.Relation.In2,
      "Load".asP -> Interpretation.Relation.In3,
      "Store".asP -> Interpretation.Relation.In3,

      "VarPointsTo".asP -> Interpretation.Relation.In2,
      "HeapPointsTo".asP -> Interpretation.Relation.In3,

      "Sum".asP -> Interpretation.Functional.Functional2
    )

    val p = Program(clauses ++ facts, interpretation)

    println(p)


    val s = new Solver(p)
    s.solve()
  }

}
