/*
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Acceptor
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Acceptor that accepts all AST nodes.
  */
case object AllAcceptor extends Acceptor {
  def accept(loc: SourceLocation): Boolean = true
}
