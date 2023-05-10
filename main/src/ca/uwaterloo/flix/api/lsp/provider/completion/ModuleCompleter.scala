/*
 * Copyright 2023 Lukas Rønn
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
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ModCompletion

object ModuleCompleter extends Completer {
  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ModCompletion] = {
    val word = context.word.split('.').toList
    root.modules.flatMap {
      case (modSym, _) =>
        if (matchesMod(modSym.ns, word)) {
          println(s"ModSym.ns: ${modSym.ns}")
          println(s"Word: $word")
          // We have a possible completion.
          // Calculate the next part of ns
          val nextNs = calculateNextNs(modSym.ns, word)
          Some(ModCompletion(nextNs))
        } else {
          // No possible completions
          None
        }
    }
  }

  private def matchesMod(ns: List[String], word: List[String]): Boolean =
    (ns, word) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => true
      case (x :: xs, y :: ys) => x.startsWith(y) && matchesMod(xs, ys)
    }

  /**
    * Calculates the next part of the nameSpace.
    *
    * If we have the following module: "A.B.C", and the user has typed "A.", it will only provide "A.B".
    *
    * @param ns   the nameSpace.
    * @param word the provided word (nameSpace)
    * @return     the already typed nameSpace and the next to come.
    */
  private def calculateNextNs(ns: List[String], word: List[String]): List[String] = {
    ns.take(word.length)
  }
}
