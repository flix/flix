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
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseEffCompletion
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.Symbol.EffectSym

object UseEffCompleter {

  def getCompletions(ctx: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(word) =>
        val effs = getLocalEffSyms(word)
        effs.map(effect => getUseEffCompletion(root.effects(effect)))
      case _ => Nil
    }
  }

  private def getLocalEffSyms(parsedWord: String)(implicit root: TypedAst.Root): List[Symbol.EffectSym] = {
    val modFragment = ModuleSymFragment.parseModuleSym(parsedWord)
    modFragment match {
      case ModuleSymFragment.Complete(modSym) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: EffectSym => sym
      }
      case ModuleSymFragment.Partial(modSym, suffix) => root.modules.getOrElse(modSym, Nil).collect {
        case sym: EffectSym if matches(sym, suffix) => sym
      }
    }
  }

  private def matches(sym: EffectSym, suffix: String): Boolean = {
    sym.name.startsWith(suffix)
  }

  private def getUseEffCompletion(decl: TypedAst.Effect): UseEffCompletion = {
    val sym = decl.sym
    Completion.UseEffCompletion(sym.toString)
  }


}
