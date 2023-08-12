/*
 * Copyright 2023 Xavier deSouza
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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseOpCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object UseOpCompleter extends Completer {
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[UseOpCompletion] = {
    stripWord(context) match {
      case Some(word) => {
        val uri = context.uri
        val validOps = root.effects.values.flatMap(_.ops).filter(op => validMatch(op, word, uri))
        validOps.map(op => getUseOpCompletion(op))
      }
      case None => None
    }
  }

  private def stripWord(ctx: CompletionContext): Option[String] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) => Some(word)
      case _ => None
    }
  }

  private def validMatch(op: TypedAst.Op, parsedWord: String, uri: String): Boolean = {
    val isLocal = uri == op.sym.loc.source.name
    val isPublic = op.spec.mod.isPublic
    val isNamespace = parsedWord.nonEmpty && parsedWord.head.isUpper
    val matches = if (isNamespace) op.sym.toString.startsWith(parsedWord)
                  else op.sym.name.startsWith(parsedWord)
    matches && (isLocal || isPublic)
  }

  private def getUseOpCompletion(op: TypedAst.Op): UseOpCompletion = {
    Completion.UseOpCompletion(op.sym.toString)
  }
}
