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
  /**
    * Returns a List of ModCompletion for modules.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ModCompletion] = {
    val validMods =
      ModuleSymFragment.parseModuleSym(context.word) match {
        // We have a complete moduleSymFragment
        case ModuleSymFragment.Complete(modSym) =>
          // Lookup in modules
          getSymsInModule(modSym)
            // Collect all subModules
            .collect {
              case sym: ModuleSym => sym
            }
        // We have a partial moduleSymFragment
        case ModuleSymFragment.Partial(modSym, suffix) =>
          // Lookup in modules
          getSymsInModule(modSym)
            // Collect all subModules that matches suffix
            .collect {
              case sym: ModuleSym if matchesMod(sym, suffix) => sym
            }
        case _ => Nil
      }
    generateModCompletions(validMods)
  }

  /**
    * Looks up in root.modules with key modSym.
    * @return List[Symbol] if the key exists, Nil otherwise.
    */
  private def getSymsInModule(modSym: Symbol.ModuleSym)(implicit root: TypedAst.Root): List[Symbol] = root.modules.getOrElse(modSym, Nil)

  /**
    * Generates ModuleCompletions
    */
  private def generateModCompletions(mods: List[Symbol.ModuleSym]): Iterable[ModCompletion] = {
    mods.map(mod => ModCompletion(mod))
  }

  /**
    * Checks if the last elem of the nameSpace matches the suffix (the word the user is currently typing).
    *
    * @param mod     the moduleSym.
    * @param suffix  the suffix of the module provided by the user.
    * @return        true, if the suffix matches the last part of nameSpace, false otherwise.
    */
  private def matchesMod(mod: ModuleSym, suffix: String): Boolean = {
    val lastNs = mod.ns.takeRight(1)
    if (lastNs == Nil) {
      false
    } else {
      lastNs(0).startsWith(suffix)
    }
  }
}
