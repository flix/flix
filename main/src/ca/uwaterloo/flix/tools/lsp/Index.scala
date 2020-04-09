package ca.uwaterloo.flix.tools.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.Expression

object Index {
  def of(e: Expression): Index = ???
}

class Index(m: Map[Int, List[(Int, Expression)]]) {

  def query(line: Int, column: Int): Option[Expression] = {
    val candidates = m.getOrElse(line, Nil).filter(p => p._1 >= column)
    // TODO: Compute some notion of span. candidates.sortBy()
    None
  }

  def ++(that: Index): Index = ???

  def +(e: Expression): Index = ???
}