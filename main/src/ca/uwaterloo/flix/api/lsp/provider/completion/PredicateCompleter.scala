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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.PredicateCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object PredicateCompleter extends Completer {

  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[PredicateCompletion] = {
    //
    // Find all definition and use sites of predicate symbols.
    //
    val defs = index.predDefs
    val uses = index.predUses

    // Merge them into one map.
    val defsAndUses = defs ++ uses

    // We select all predicate symbols:
    // - from the same source file,
    // - and with a non-zero arity.
    for (
      (pred, arityAndLocs) <- defsAndUses.m;
      (arity, loc) <- arityAndLocs;
      if arity > 0 && loc.source.name == context.uri
    ) yield Completion.PredicateCompletion(pred.name, arity)
  }
}
