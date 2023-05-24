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
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ModCompletion
import ca.uwaterloo.flix.language.ast.Symbol.ModuleSym

object ModuleCompleter extends Completer {

  def getCompletions(ctx: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ModCompletion] = {
    val nestedModules = getNestedModules(ctx)
    nestedModules.map(mod => ModCompletion(mod))
  }

  private def getNestedModules(ctx: CompletionContext)(implicit root: TypedAst.Root): List[Symbol.ModuleSym] = {
    ModuleSymFragment.parseModuleSym(ctx.word) match {
      case ModuleSymFragment.Complete(modSym) =>
        root.modules.getOrElse(modSym, Nil).collect {
          case sym: ModuleSym => sym
        }
      case ModuleSymFragment.Partial(modSym, suffix) =>
        root.modules.getOrElse(modSym, Nil).collect {
          case sym: ModuleSym if matches(sym, suffix) => sym
        }
      case _ => Nil
    }
  }

  /**
    * Returns `true` if the given module `sym` matches the given `suffix`.
    *
    * (Aaa.Bbb.Ccc, Cc) => true
    * (Aaa.Bbb.Ccc, Dd) => false
    * (/, Cc)           => true
    */
  private def matches(sym: Symbol.ModuleSym, suffix: String): Boolean = {
    if (sym.isRoot) {
      true
    } else {
      sym.ns.last.startsWith(suffix) // We know that ns cannot be empty because it is not the root.
    }
  }
}
