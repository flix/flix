package examples

import impl.Runner
import impl.logic.{HornClause, Predicate, Term, _}
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object IFDS {
  def main(args: Array[String]): Unit = {


    /*

  
inputs:
esharp-intra: D x N x D -> Bool // includes call-return edges
esharp-cs:    N x D x P x D -> Bool
esharp-er:    P x D x N x D -> Bool
callgraph:    N x P -> Bool
startnode:    P x N -> Bool (or really P -> N)
endnode:      P x N -> Bool (or really P -> N)
cfg:          N x N -> Bool

pathedge(d1, m, d3) :- pathedge(d1, n, d2), esharp-intra(d2, n, d3), cfg(n, m)
pathedge(d1, m, d3) :- pathedge(d1, n, d2), summaryedge(d2, n, d3), cfg(n, m)
pathedge(d3, sc, d3) :- 
  pathedge(d1, n, d2), esharp-cs(n, d2, target, d3),
  callgraph(n, target), startnode(target, sc)
summaryedge(d4, c, d5) :- 
  esharp-cs(c, d4, target, d1), pathedge(d1, n, d2), esharp-er(target, d2, c, d5),
  callgraph(c, target), endnode(target, n)

   */

    val clauses = List(
      HornClause(Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("m"), Term.Variable("d3"))), List(
        Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("n"), Term.Variable("d2"))),
        Predicate("EsharpIntra", List(Term.Variable("d2"), Term.Variable("n"), Term.Variable("d3"))),
        Predicate("CFG", List(Term.Variable("n"), Term.Variable("m"))))),

      HornClause(
        head = Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("m"), Term.Variable("d3"))),
        body = List(
          Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("n"), Term.Variable("d2"))),
          Predicate("SummaryEdge", List(Term.Variable("d2"), Term.Variable("n"), Term.Variable("d3"))),
          Predicate("CFG", List(Term.Variable("n"), Term.Variable("m"))))),

      HornClause(Predicate("PathEdge", List(Term.Variable("d3"), Term.Variable("sc"), Term.Variable("d3"))), List(
        Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("n"), Term.Variable("d2"))),
        Predicate("EsharpCS", List(Term.Variable("n"), Term.Variable("d2"), Term.Variable("target"), Term.Variable("d3"))),
        Predicate("CallGraph", List(Term.Variable("n"), Term.Variable("target"))),
        Predicate("StartNode", List(Term.Variable("target"), Term.Variable("sc")))
      )),

      HornClause(Predicate("SummaryEdge", List(Term.Variable("d4"), Term.Variable("c"), Term.Variable("d5"))), List(
        Predicate("EsharpCS", List(Term.Variable("c"), Term.Variable("d4"), Term.Variable("target"), Term.Variable("d1"))),
        Predicate("PathEdge", List(Term.Variable("d1"), Term.Variable("n"), Term.Variable("d2"))),
        Predicate("EsharpER", List(Term.Variable("target"), Term.Variable("d2"), Term.Variable("c"), Term.Variable("d5"))),
        Predicate("CallGraph", List(Term.Variable("c"), Term.Variable("target"))),
        Predicate("StartNode", List(Term.Variable("target"), Term.Variable("n")))
      )
      )
    )

    val cfg = Map(
      "main_start" -> "main_call",
      "main_call" -> "main_end",
      "proc_start" -> "proc_end"
    ).map { case (a, b) => (Term.Constructor0(a), Term.Constructor0(b))}.toMap

    val facts = List(
      Predicate("CFG", List(Term.Constructor0("main_start"), Term.Constructor0("main_call"))),
      Predicate("CFG", List(Term.Constructor0("main_call"), Term.Constructor0("main_end"))),
      Predicate("CFG", List(Term.Constructor0("proc_start"), Term.Constructor0("proc_end"))),

      Predicate("CallGraph", List(Term.Constructor0("main_call"), Term.Constructor0("proc"))),

      Predicate("StartNode", List(Term.Constructor0("proc"), Term.Constructor0("proc_start"))),

      Predicate("EndNode", List(Term.Constructor0("proc"), Term.Constructor0("proc_end"))),

      Predicate("EsharpIntra", List(Term.Constructor0("0"), Term.Constructor0("main_start"), Term.Constructor0("0"))),
      Predicate("EsharpCS", List(Term.Constructor0("main_call"), Term.Constructor0("0"), Term.Constructor0("proc"), Term.Constructor0("0"))),
      Predicate("EsharpER", List(Term.Constructor0("proc"), Term.Constructor0("0"), Term.Constructor0("main_call"), Term.Constructor0("0"))),

      Predicate("EsharpIntra", List(Term.Constructor0("0"), Term.Constructor0("proc_start"), Term.Constructor0("0"))),

      Predicate("PathEdge", List(Term.Constructor0("0"), Term.Constructor0("main_start"), Term.Constructor0("0")))

    ).map(HornClause(_, List()))


    val interpretations = List(
      "PathEdge",
      "SummaryEdge",
      "CFG",
      "CallGraph",
      "StartNode",
      "EndNode",
      "EsharpIntra",
      "EsharpCS",
      "EsharpER"
    ).map {
      name: String => (Symbol.PredicateSymbol(name), Interpretation.Relation)
    }.toMap

    val hints = List(
      "PathEdge",
      "SummaryEdge",
      "CFG",
      "CallGraph",
      "StartNode",
      "EndNode",
      "EsharpIntra",
      "EsharpCS",
      "EsharpER"
    ).map {
      name: String => (Symbol.PredicateSymbol(name), Hint(Representation.Data))
    }.toMap

    val program = Program(facts ::: clauses, interpretations, Map.empty)

    Runner.run(program, hints)
  }
}