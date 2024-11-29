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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseSignatureCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object UseSignatureCompleter {
  /**
   * Returns an Iterable of UseSignatureCompletions for the completer.
   */
  def getCompletions(context: CompletionContext)(implicit root: TypedAst.Root): Iterable[UseSignatureCompletion] = {
    stripWord(context) match {
      case Some(word) =>
        val uri = context.uri
        val sigs = root.sigs.values.filter(s => validMatch(s, word, uri))
        sigs.map(sig => getUseSigCompletion(sig))
      case _ => Nil
    }
  }

  /**
   * Strips the current word of `use` to enable completion checking.
   */
  private def stripWord(ctx: CompletionContext): Option[String] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) => Some(word)
      case _ => None
    }
  }

  /**
   * Returns 'true' if `parsedWord` matches a valid signature.
   */
  private def validMatch(sig: TypedAst.Sig, parsedWord: String, uri: String): Boolean = {
    val isLocal = uri == sig.sym.loc.source.name
    val isPublic = sig.spec.mod.isPublic
    val isNamespace = parsedWord.nonEmpty && parsedWord.head.isUpper
    val matches = if (isNamespace) sig.sym.toString.startsWith(parsedWord)
                  else             sig.sym.name.startsWith(parsedWord)
    matches && (isLocal || isPublic)
  }

  private def getUseSigCompletion(sig: TypedAst.Sig): UseSignatureCompletion = {
    Completion.UseSignatureCompletion(sig.sym.toString)
  }
}
