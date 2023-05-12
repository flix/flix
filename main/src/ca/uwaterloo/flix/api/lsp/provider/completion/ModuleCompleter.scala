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

  /**
    * Gets completions for modules when not having a fqn
    */
  private def getCurrentModuleCompletion(word: List[String])(implicit root: TypedAst.Root): Iterable[ModCompletion] = {
    // The user has provided a subWord
    // We know that there is at least one dot, so we split the word and
    // make a fqn from the everything but the the last (hence it is the subWord).
    val fqnWithOutLastWord = word.dropRight(1)
    val subWord = word.takeRight(1)(0)
    val moduleSym = mkModuleSym(fqnWithOutLastWord)

    if (fqnWithOutLastWord.isEmpty) {
      // Case 1: We don't have a dot, therefore the module we are looking for only has ns of length 1
      root.modules.keys.flatMap {
        mod =>
          if (mod.ns.length == 1 && matchesMod(mod, subWord)) {
            Some(ModCompletion(mod))
          } else {
            None
          }
      }
    } else {
      // Case 2: We have a dot, make a fqn to find the next part of NS the user is typing
      root.modules.get(moduleSym) match {
        case None =>
          // Not a valid module
          Nil
        case Some(syms) =>
          // Is a valid module, get all possible moduleCompletions that matches subWord
          syms.flatMap {
            case mod: ModuleSym =>
              if (matchesMod(mod, subWord)) {
                // Case 3.1: The subWord matches a valid module
                Some(ModCompletion(mod))
              } else {
                // Case 3.2: The subWord doesn't match
                None
              }
            case _ => None
          }
      }
    }
  }

  /**
    * Gets completions for modules when having a fqn
    */
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
    * @return        true, if the subWord matches the last part of nameSpace, false otherwise.
    */
  private def matchesMod(mod: ModuleSym, subWord: String): Boolean = {
    val lastNs = mod.ns.takeRight(1)(0)
    lastNs.startsWith(subWord)
  }
}
