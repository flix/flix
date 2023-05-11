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
import ca.uwaterloo.flix.language.ast.Symbol.ModuleSym

object ModuleCompleter extends Completer {
  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ModCompletion] = {
    // We don't know if the user has provided a fqn, or is currently writing it so we have to try both cases
    val word = context.word.split('.').toList

    getCurrentModuleCompletion(word) ++
      getNextModuleCompletion(word)
  }

  private def getCurrentModuleCompletion(word: List[String])(implicit root: TypedAst.Root): Iterable[ModCompletion] = {
    // The user has provided a subWord
    // We know that there is at least one dot, so we split the word and
    // make a fqn from the everything but the the last (hence it is the subWord).
    val fqnWithOutLastWord = word.dropRight(1)
    val lastWord = word.takeRight(1).toString()
    val moduleSym = mkModuleSym(fqnWithOutLastWord)

    root.modules.flatMap {
      case (mod, syms) =>
        if (fqnWithOutLastWord.isEmpty) {
          if (matchesMod(mod, lastWord)) {
            Some(ModCompletion(mod))
          } else {
            None
          }
        } else {
          if (mod.equals(moduleSym)) {
            syms.flatMap {
              case mod: ModuleSym =>
                if (matchesMod(mod, lastWord)) {
                  Some(ModCompletion(mod))
                } else {
                  None
                }
              case _ => None
            }
          } else {
            None
          }
        }
    }
  }

  private def getNextModuleCompletion(word: List[String])(implicit root: TypedAst.Root): Iterable[ModCompletion] = {
    val moduleSym = mkModuleSym(word)
    root.modules.get(moduleSym) match {
      case None =>
        // Not a valid module
        Nil
      case Some(sym) =>
        // Is a valid module, get all possible moduleCompletions
        sym.flatMap {
          case mod: ModuleSym => Some(ModCompletion(mod))
          case _ => None
        }
    }
  }

  /**
    * Generates an moduleSym with the given nameSpace.
    *
    * @param fqn the fully qualified name as a List[String].
    * @return    the module symbol for the given fully qualified name.
    */
  private def mkModuleSym(fqn: List[String]): ModuleSym = new ModuleSym(fqn)

  /**
    * Checks if the last elem of the nameSpace matches the subWord (the word the user is currently typing).
    *
    * @param mod     the moduleSym.
    * @param subWord the subWord provided by the user.
    * @return        true, if the subWord matches the modulesNamespace
    */
  private def matchesMod(mod: ModuleSym, subWord: String): Boolean = {
    val lastNs = mod.ns.takeRight(1).toString()
    lastNs.startsWith(subWord)
  }
}
