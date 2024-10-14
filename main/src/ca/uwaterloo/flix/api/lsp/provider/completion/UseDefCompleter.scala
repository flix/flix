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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseDefCompletion
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object UseDefCompleter {
  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[UseDefCompletion] = {
    stripWord(context) match {
      case Some(word) =>
        val defs = getLocalDefSyms(word)
        defs.map(df => getUseDefCompletion(root.defs(df)))
      case _ => Nil
    }
  }

  private def getLocalDefSyms(parsedWord: String)(implicit root: TypedAst.Root): List[Symbol.DefnSym] = {
    val modFragment = ModuleSymFragment.parseModuleSym(parsedWord)
      modFragment match {
      case ModuleSymFragment.Complete(modSym) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: DefnSym => sym
      }
      case ModuleSymFragment.Partial(modSym, suffix) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: DefnSym if matches(sym, suffix) => sym
      }
    }
  }

  private def stripWord(ctx: CompletionContext): Option[String] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) => Some(word)
      case _ => None
    }
  }
  private def matches(sym: DefnSym, suffix: String): Boolean = {
    sym.name.startsWith(suffix)
  }

  private def getUseDefCompletion(decl: TypedAst.Def): UseDefCompletion = {
    val sym = decl.sym
    Completion.UseDefCompletion(sym.toString)
  }
}
