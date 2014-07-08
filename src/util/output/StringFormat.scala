package util.output

import impl.logic._
import impl.runtime.Solver
import syntax.Symbols._
import syntax.Terms._
import syntax.Values._

object StringFormat {
  def format(p: Program): String = p.facts.map(format).mkString("\n") + "\n\n" + p.rules.map(format).mkString("\n")

  def format(h: HornClause): String =
    if (h.isFact)
      format(h.head) + "."
    else
      format(h.head) + " :- " + h.body.map(format).mkString(", ") + "."

  def format(p: Predicate): String = p.name.fmt + "(" + p.terms.map(t => t.fmt).mkString(", ") + ")"

  def printSolution(s: Solver): Unit = {
    println("*** Solution ***")
    for ((p, vs) <- s.relation1; v1 <- vs) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for ((p, vs) <- s.relation2; (v1, v2) <- vs) {
      println(p.fmt + "(" + v1.fmt + "," + v2.fmt + ").")
    }

    for ((p, v1) <- s.map1) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for ((p, (v1, v2)) <- s.map2) {
      println(p.fmt + "(" + v1.fmt + "," + v2.fmt + ").")
    }
  }
}
