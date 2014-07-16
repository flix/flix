package examples

import impl.Runner
import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

import scala.util.Random

object RandomGraph01 {

  val NumberOfVertices = 25
  val NumberOfEdges = 250

  val Random = new Random()

  val Vertices = (0 to NumberOfVertices) map (_ => Random.nextInt(NumberOfVertices))
  val Edges = (0 to NumberOfEdges) map (_ => (Random.nextInt(NumberOfVertices), Random.nextInt(NumberOfVertices)))

  def main(args: Array[String]): Unit = {

    val EdgeSymbol = Symbol.PredicateSymbol("Edge")
    val CycleSymbol = Symbol.PredicateSymbol("Cycle")

    val facts = Edges.toList map {
      case (x, y) => HornClause(Predicate(EdgeSymbol, List(Term.Int(x), Term.Int(y))))
    }

    val clauses = List(
      // E(x, z) :- E(x, y), E(y, z).
      HornClause(head =
        Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("z"))),
        body = List(
          Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("y"))),
          Predicate(EdgeSymbol, List(Term.Variable("y"), Term.Variable("z")))
        )),

      // C(x) :- E(x, x).
      HornClause(head =
        Predicate(CycleSymbol, List(Term.Variable("x"))),
        body = List(
          Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("x")))
        ))
    )

    val interpretations = Map(
      EdgeSymbol -> Interpretation.Relation,
      CycleSymbol -> Interpretation.Relation
    )

    val hints = Map(
      EdgeSymbol -> Hint(Representation.Data),
      CycleSymbol -> Hint(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }

}
