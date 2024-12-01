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
