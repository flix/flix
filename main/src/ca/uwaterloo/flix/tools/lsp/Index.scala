package ca.uwaterloo.flix.tools.lsp

import ca.uwaterloo.flix.language.ast.TypedAst.Expression

object Index {
  def of(e: Expression): Index = ???
}

trait Index {
  def ++(that: Index): Index = ???

  def +(e: Expression): Index = ???
}