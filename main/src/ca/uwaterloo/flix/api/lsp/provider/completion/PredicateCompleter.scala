/*
 * Copyright 2022 Paul Butcher
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

import ca.uwaterloo.flix.api.lsp.{CompletionItem, Index}
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.language.ast.TypedAst

object PredicateCompleter {

  /**
    * Returns a list of completion for predicates
    */
  def getPredicateCompletions()(implicit context: CompletionContext, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    List(
      index.predDefs.m.concat(index.predUses.m)
        .foreach {
          case (pred, locs) =>
            val priority: String => String = if (locs.exists(loc => loc.source.name == context.uri)) Priority.boost else Priority.low
            val name = pred.name
            (name, priority(name))
        }) map Completion.PredicateCompletion
  }
}
