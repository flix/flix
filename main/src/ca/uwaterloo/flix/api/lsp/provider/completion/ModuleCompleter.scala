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
    val (modSym, subWord) = CompletionUtils.generateModSymAndSubwordFromContext(context)

    getModuleCompletion(modSym, subWord)
  }

  /**
    * Get all module completions
    */
  private def getModuleCompletion(modSym: Symbol.ModuleSym, subWord: String)(implicit root: TypedAst.Root): Iterable[ModCompletion] = {
    // Use fqn to lookup in modules and get all syms
    val symsInModule = root.modules.getOrElse(modSym, Nil)

    // Get all modules that matches word
    val validModsInModule = symsInModule.collect {
      case sym: ModuleSym if matchesMod(sym, subWord) => sym
    }

    // Generate completions
    validModsInModule.map(mod => ModCompletion(mod))
  }

  /**
    * Checks if the last elem of the nameSpace matches the subWord (the word the user is currently typing).
    *
    * @param mod     the moduleSym.
    * @param subWord the subWord provided by the user.
    * @return        true, if the subWord matches the last part of nameSpace, false otherwise.
    */
  private def matchesMod(mod: ModuleSym, subWord: String): Boolean = {
    val lastNs = mod.ns.takeRight(1)(0)
    lastNs.startsWith(subWord)
  }
}
