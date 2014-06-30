package util.output

import impl.logic._

object StringFormat {
  def format(p: Program): String = p.facts.map(format).mkString("\n") + "\n\n" + p.rules.map(format).mkString(", ")

  def format(h: HornClause): String = s"${format(h.head)} :- ${h.body.map(format(_).mkString(", "))}\n"

  def format(p: Predicate): String = format(p.name) + "(" + p.terms.map(format) + ")"

  def format(t: Term): String = t.toString

  def format(s: Symbol): String = s match {
    case Symbol.PredicateSymbol(x) => x
    case Symbol.NamedSymbol(x) => x
    case Symbol.VariableSymbol(x) => x
  }
}
