/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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

import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportMethodCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ImportMethodCompleter extends Completer {
  /**
    * Returns a List of Completion for importMethod.
    */
  override def getCompletions(implicit context: CompletionContext, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[ImportMethodCompletion] =
    null
}
