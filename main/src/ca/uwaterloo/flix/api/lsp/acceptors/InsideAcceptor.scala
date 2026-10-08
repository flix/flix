/*
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Visitor.inside
import ca.uwaterloo.flix.api.lsp.{Acceptor, Position}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.SourceName

/**
  * Acceptor that accepts an AST node if it contains a given position.
  *
  * [[InsideAcceptor]] accepts an AST if its [[SourceLocation]] is within the source named `name`
  * and the `pos` is within the [[SourceLocation]] of the AST.
  *
  * @param name the name of the source that the AST node [[SourceLocation]] must be in to be accepted.
  * @param pos the [[Position]] that must be within the AST node's [[SourceLocation]] for the node to be accepted.
  */
case class InsideAcceptor(name: SourceName, pos: Position) extends Acceptor {
  def accept(loc: SourceLocation): Boolean = inside(name, pos)(loc)
}
