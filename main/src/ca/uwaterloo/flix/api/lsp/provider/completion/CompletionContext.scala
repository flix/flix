/*
 * Copyright 2023 Magnus Madsen, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.{Position, Range}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext

/**
  * Represents a completion context.
  *
  * @param uri          Source file URI (from client)
  * @param pos          The position in LSP
  * @param range        Start and end position of the word underneath (or alongside) the cursor
  * @param sctx         The syntactic context.
  * @param word         The word underneath (or alongside) the cursor
  * @param previousWord The word before the above (note that this may be on either the current or previous line)
  * @param prefix       The text from the start of the line up to the cursor
  * @param errors       The current compilation errors
  */
case class CompletionContext(uri: String, pos: Position, range: Range, sctx: SyntacticContext, word: String, previousWord: String, prefix: String, errors: List[CompilationMessage]) {
  def isCurrent: Boolean = errors.isEmpty
}
