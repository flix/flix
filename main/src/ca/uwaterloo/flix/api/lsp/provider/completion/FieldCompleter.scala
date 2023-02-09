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
import ca.uwaterloo.flix.language.ast.TypedAst

object FieldCompleter {

  /**
    * Gets completions for record fields
    */
  def getFieldCompletions()(implicit context: CompletionContext, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    // Do not get field completions if we are importing or using.
    if (root == null || context.prefix.contains("import") || context.prefix.contains("use")) {
      return Nil
    }

    val regex = raw"(.*)[.].*".r

    context.word match {
      case regex(prefix) =>
          index.fieldDefs.m.concat(index.fieldUses.m)
            .filter { case (_, locs) => locs.exists(loc => loc.source.name == context.uri) }
            .foldLeft[List[CompletionItem]](Nil) {
              case (acc, (field, _)) =>
                Completion.FieldCompletion(s"$prefix.${field.name}", context).toCompletionItem :: acc
            }
      case _ => Nil
    }
  }
}
