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
import ca.uwaterloo.flix.api.lsp.{Entity, Index}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.VarCompletion
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.Symbol

object VarCompleter {
  /**
    * Returns a List of Completion for var.
    */
  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[VarCompletion] = {
    // Find all local variables in the current uri with a given range.
    index.queryWithRange(context.uri, queryLine = context.range.start.line, beforeLine = 20, afterLine = 10).collect {
      case Entity.LocalVar(sym, tpe) => Completion.VarCompletion(sym, tpe)
      case Entity.FormalParam(fparam) => Completion.VarCompletion(fparam.sym, fparam.tpe)
    }.filter(comp => matchesVar(comp.sym, context.word))
  }

  /**
    * Checks that the varSym matches the word that the users is typing.
    *
    * @param sym  the varSym.
    * @param word the current word.
    * @return     true, if the var matches word, false otherwise.
    */
  private def matchesVar(sym: Symbol.VarSym, word: String): Boolean = sym.toString.startsWith(word)
}
