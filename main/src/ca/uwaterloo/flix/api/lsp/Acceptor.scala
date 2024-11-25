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
