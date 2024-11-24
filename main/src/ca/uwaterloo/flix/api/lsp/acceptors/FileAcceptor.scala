package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Acceptor
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Acceptor that accepts all AST nodes whose `SourceLocation` is within
  * the file given by the path `uri`.
  *
  * @param uri  the path of the file that an AST node [[SourceLocation]] must be within to be accepted.
  */
case class FileAcceptor(uri: String) extends Acceptor {
  def accept(loc: SourceLocation): Boolean = uri == loc.source.name
}
