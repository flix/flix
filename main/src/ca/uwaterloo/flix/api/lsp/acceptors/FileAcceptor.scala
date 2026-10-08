/*
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Acceptor
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.shared.SourceName

/**
  * Acceptor that accepts all AST nodes whose `SourceLocation` is within the source named `name`.
  *
  * @param name the name of the source that an AST node [[SourceLocation]] must be within to be accepted.
  */
case class FileAcceptor(name: SourceName) extends Acceptor {
  def accept(loc: SourceLocation): Boolean = name == loc.source.sourceName
}
