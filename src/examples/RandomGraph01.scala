package examples

import impl.logic._
import impl.runtime.Runner
import syntax.Symbols._

import scala.util.Random

object RandomGraph01 {

  val NumberOfVertices = 100
  val NumberOfEdges = 10000

  val Random = new Random()

  val Vertices = (0 to NumberOfVertices) map (_ => Random.nextInt(NumberOfVertices))
  val Edges = (0 to NumberOfEdges) map (_ => (Random.nextInt(NumberOfVertices), Random.nextInt(NumberOfVertices)))

  def main(args: Array[String]): Unit = {

    val EdgeSymbol = Symbol.PredicateSymbol("Edge")

    val facts = Edges.toList map {
      case (x, y) => HornClause(Predicate(EdgeSymbol, List(Term.Constant(Value.Int(x)), Term.Constant(Value.Int(y)))))
    }

    // E(x, z) :- E(x, y), E(y, z).
    val clauses = List(
      HornClause(head =
        Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("z"))),
        body = List(
          Predicate(EdgeSymbol, List(Term.Variable("x"), Term.Variable("y"))),
          Predicate(EdgeSymbol, List(Term.Variable("y"), Term.Variable("z")))
        ))
    )

    val interpretations = Map(
      EdgeSymbol -> Interpretation.Relation(Representation.Data)
    )

    val program = Program(facts ::: clauses, interpretations)

    Runner.run(program)
  }

}
