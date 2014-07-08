package util.output

import impl.logic._
import impl.runtime.Solver

object StringFormat {
  def format(p: Program): String = p.facts.map(format).mkString("\n") + "\n\n" + p.rules.map(format).mkString("\n")

  def format(h: HornClause): String =
    if (h.isFact)
      format(h.head) + "."
    else
      format(h.head) + " :- " + h.body.map(format).mkString(", ") + "."

  def format(p: Predicate): String = format(p.name) + "(" + p.terms.map(format).mkString(", ") + ")"

  def format(t: Term): String = t match {
    case Term.Constant(v) => format(v)
    case Term.Variable(Symbol.VariableSymbol(v)) => v
    case tt => tt.toString
  }

  def format(v: Value): String = v match {
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.String(s) => s
    case Value.Constructor0(s) => format(s)
    case Value.Constructor1(s, v1) => format(s) + "(" + format(v1) + ")"
    case Value.Constructor2(s, v1, v2) => format(s)
    case Value.Constructor3(s, v1, v2, v3) => format(s)
    case Value.Constructor4(s, v1, v2, v3, v4) => format(s)
    case Value.Constructor5(s, v1, v2, v3, v4, v5) => format(s)

    case vv => vv.toString
  }

  def format(s: Symbol): String = s match {
    case Symbol.FunctionSymbol(x) => x
    case Symbol.PredicateSymbol(x) => x
    case Symbol.NamedSymbol(x) => x
    case Symbol.VariableSymbol(x) => x
  }

  def printSolution(s: Solver): Unit = {
    println("*** Solution ***")
    for ((p, vs) <- s.relation1; v1 <- vs) {
      println(format(p) + "(" + format(v1) + ").")
    }
    for ((p, vs) <- s.relation2; (v1, v2) <- vs) {
      println(format(p) + "(" + format(v1) + "," + format(v2) + ").")
    }

    for ((p, v1) <- s.map1) {
      println(format(p) + "(" + format(v1) + ").")
    }
    for ((p, (v1, v2)) <- s.map2) {
      println(format(p) + "(" + format(v1) + "," + format(v2) + ").")
    }
  }
}
