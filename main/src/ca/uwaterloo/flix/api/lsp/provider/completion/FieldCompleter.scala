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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object FieldCompleter extends Completer {

  /**
    * Returns a List of Completion for fields.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[FieldCompletion] = {
    // Do not get field completions if we are importing or using.
    if (context.prefix.contains("import") || context.prefix.contains("use")) {
      return Nil
    }

    val regex = raw"(.*)[.].*".r

    context.word match {
      case regex(prefix) =>
        index.fieldDefs.m.concat(index.fieldUses.m)
          .filter { case (_, locs) => locs.exists(loc => loc.source.name == context.uri) }
          .map {
            case (field, _) =>
              Completion.FieldCompletion(field, prefix)
          }
      case _ => Nil
    }
  }
}
