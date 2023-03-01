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
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object OpCompleter extends Completer{
  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[OpCompletion] = {
    if (root.isEmpty || context.previousWord != "do") {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    root.get.effects.values.flatMap(_.ops).filter(matchesOp(_, word, uri))
      .flatMap(decl =>
        if (CompletionProvider.canApplySnippet(decl.spec.fparams))
          Some(Completion.OpCompletion(decl, context, flix))
        else
          None
      )
  }

  /**
    * Returns `true` if the given effect operation `op` should be included in the suggestions.
    */
  private def matchesOp(op: TypedAst.Op, word: String, uri: String): Boolean = {
    val isPublic = op.spec.mod.isPublic
    val isNamespace = word.nonEmpty && word.head.isUpper
    val isMatch = if (isNamespace)
      op.sym.toString.startsWith(word)
    else
      op.sym.name.startsWith(word)
    val isInFile = op.sym.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }
}
