/*
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.api.lsp.acceptors

import ca.uwaterloo.flix.api.lsp.Visitor.inside
import ca.uwaterloo.flix.api.lsp.{Acceptor, Position}
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Acceptor that accepts an AST node if it contains a given position.
  *
  * [[InsideAcceptor]] accepts an AST if it's [[SourceLocation]] is within the file given by `uri`
  * and the `pos` is within the [[SourceLocation]] of the AST.
  *
  * @param uri the path to the file that the AST node [[SourceLocation]] must be in to be accepted.
  * @param pos the [[Position]] that must be within the AST node's [[SourceLocation]] for the node to be accepted.
  */
case class InsideAcceptor(uri: String, pos: Position) extends Acceptor {
  def accept(loc: SourceLocation): Boolean = inside(uri, pos)(loc)
}
