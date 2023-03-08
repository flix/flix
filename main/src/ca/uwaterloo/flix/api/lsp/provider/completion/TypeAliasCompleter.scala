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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TypeAliasCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.TypedAst

object TypeAliasCompleter extends Completer {
  /**
    * Returns a List of Completion for alias types.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[TypeAliasCompletion] = {
    root.typeAliases.map {
      case (_, t) =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)(context)
        Completion.TypeAliasCompletion(t.sym, formatTParams(t.tparams), priorityBoostForTypes(internalPriority(name))(context),
          TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"), Some(t.doc.text))
    }
  }
}
