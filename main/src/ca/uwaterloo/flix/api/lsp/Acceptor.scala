package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SourceLocation

trait Acceptor {
  /**
    * Defines whether an AST node is visited based on its [[SourceLocation]]
    *
    * @param loc  [[SourceLocation]] of the AST node
    * @return     true if the AST node should be visited, `false` otherwise
    */
  def accept(loc: SourceLocation): Boolean
}
