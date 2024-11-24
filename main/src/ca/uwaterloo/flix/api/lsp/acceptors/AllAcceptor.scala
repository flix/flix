package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Acceptor
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Acceptor that accepts all AST nodes.
  */
case object AllAcceptor extends Acceptor {
  def accept(loc: SourceLocation): Boolean = true
}
