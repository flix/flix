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
import ca.uwaterloo.flix.api.lsp.{Index, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.api.lsp.provider.CompletionPriority
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.WithCompletion
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.phase.{Deriver, Resolver}

object WithCompleter extends Completer {
  /**
    * Returns a List of Completion based on with type class constraints.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[WithCompletion] = {
    /*
     * When used with `enum`, `with` needs to be treated differently: we should only show derivable
     * type classes, and we shouldn't include the type parameter
     */

    val enumPattern = raw"\s*enum\s+(.*\s)wi?t?h?\s?.*".r
    val withPattern = raw"\s*(def|instance|class)\s+(.*\s)wi?t?h?\s?.*".r
    val wordPattern = "wi?t?h?".r

    val currentWordIsWith = wordPattern matches context.word

    if (enumPattern matches context.prefix) {
      for {
        (_, trt) <- root.traits
        sym = trt.sym
        if Deriver.DerivableSyms.contains(sym)
        name = sym.toString
        completion = if (currentWordIsWith) s"with $name" else name
      } yield {
        Completion.WithCompletion(completion, CompletionPriority.highest(name), TextEdit(context.range, completion),
          Some(trt.doc.text), InsertTextFormat.PlainText)
      }
    } else if (withPattern.matches(context.prefix) || currentWordIsWith) {
      root.traits.map {
        case (_, trt) =>
          val name = trt.sym.toString
          val hole = "${1:t}"
          val application = s"$name[$hole]"
          val completion = if (currentWordIsWith) s"with $application" else application
          val label = if (currentWordIsWith) s"with $name[...]" else s"$name[...]"
          Completion.WithCompletion(label, CompletionPriority.highest(name), TextEdit(context.range, completion),
            Some(trt.doc.text), InsertTextFormat.Snippet)
      }
    } else {
      Nil
    }
  }
}
