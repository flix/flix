package util.output

import impl.logic._

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
    case Value.String(s) => s
    case vv => vv.toString
  }

  def format(s: Symbol): String = s match {
    case Symbol.PredicateSymbol(x) => x
    case Symbol.NamedSymbol(x) => x
    case Symbol.VariableSymbol(x) => x
  }
}
