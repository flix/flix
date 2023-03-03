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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EffectCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object EffectCompleter extends Completer {
  /**
    * Returns a List of Completion for effects.
    */
  override def getCompletions(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[EffectCompletion] = {
    if (root.isEmpty) {
      return Nil
    }

    // Boost priority if there is `\` or `\ {` immediately before the word the user is typing
    val effSetPrefix = raw".*\\\s*\{?\s*\S*".r

    val priority = if (context.previousWord == "without") {
      // If the last word is `without`, we can be very sure an effect is coming.
      Priority.high _
    } else if (effSetPrefix.matches(context.prefix) || context.previousWord == "with") {
      // If the last word is `with` or looks like `\` or `\ {`, it is likely an effect but could be something else.
      Priority.boost _
    } else {
      // Otherwise it's probably not an effect.
      Priority.low _
    }

    root.get.effects.map {
      case (_, t) =>
        val name = t.sym.name
        Completion.EffectCompletion(name, priority(name), Some(t.doc.text), context)
    }
  }
}
