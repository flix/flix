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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.mkModuleSym
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.Symbol.{EnumSym, ModuleSym}
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object EnumCompleter extends Completer {
  /**
    * Returns a List of Completion for enums.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[EnumCompletion] = {
    // We use a fqn to lookup in the modules
    // We therefore need to split the context.word at dots
    val word = context.word.split('.').toList
    val (fqn, subWord) = {
      // If the word provided ends with a dot, we should list all enums in that namespace
      if (context.word.takeRight(1) == ".") {
        (mkModuleSym(word), "")
      } else {
        (mkModuleSym(word.dropRight(1)), word.takeRight(1)(0))
      }
    }

    getEnumCompletion(context, fqn, subWord)
  }

  /**
    * Get all Enum completions
    */
  private def getEnumCompletion(context: CompletionContext, fqn: ModuleSym, subWord: String)(implicit root: TypedAst.Root): Iterable[EnumCompletion] = {
    // Use fqn to lookup in modules
    root.modules.get(fqn) match {
      case None =>
        // Not a valid module, therefore no enums
        Nil
      case Some(syms) =>
        syms.flatMap {
          case enumSym: Symbol.EnumSym =>
            // Collect all enums
            if (matchesEnum(enumSym, subWord)) {
              // Check if the word provided matches a valid enum
              root.enums.get(enumSym) match {
                case None => // not possible
                  None
                case Some(enm) =>
                  Some(enumCompletion(context, enumSym, enm))
              }
            } else {
              None
            }
          case _ => None
        }
    }
  }

  /**
    * Generates an EnumCompletion
    */
  private def enumCompletion(context: CompletionContext, enumSym: EnumSym, enm: TypedAst.Enum): EnumCompletion = {
    val internalPriority = getInternalPriority(enm.loc, enm.sym.namespace)(context)
    val name = enumSym.name
    Completion.EnumCompletion(enumSym, formatTParams(enm.tparams), priorityBoostForTypes(internalPriority(name))(context),
      TextEdit(context.range, s"${enumSym.toString}${formatTParamsSnippet(enm.tparams)}"), Some(enm.doc.text))
  }

  /**
    * Checks that the enumSym matches the word that the users is typing.
    *
    * @param sym  the enumSym.
    * @param word the current word.
    * @return     true, if the enum matches word, false otherwise.
    */
  private def matchesEnum(sym: Symbol.EnumSym, word: String): Boolean = sym.name.startsWith(word)
}
