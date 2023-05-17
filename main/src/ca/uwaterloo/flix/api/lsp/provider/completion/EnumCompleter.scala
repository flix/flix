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
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object EnumCompleter extends Completer {
  /**
    * Returns a List of Completion for enums.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[EnumCompletion] = {
    val (modSym, subWord) = CompletionUtils.generateModSymAndSubwordFromContext(context)

    getEnumCompletion(context, modSym, subWord)
  }

  /**
    * Get all Enum completions
    */
  private def getEnumCompletion(context: CompletionContext, modSym: Symbol.ModuleSym, subWord: String)(implicit root: TypedAst.Root): Iterable[EnumCompletion] = {
    // Use fqn to lookup in modules and get all enums
    val enumsInModule = root.modules.getOrElse(modSym, Nil)

    // Get all enums that matches word
    val enumsMatchingSubword = enumsInModule.collect {
      case sym: EnumSym if matchesEnum(sym, subWord) => sym
    }

    // Generate completions
    enumsMatchingSubword.flatMap {
      enumSym =>
        root.enums.get(enumSym) match {
          case None => // not possible
            None
          case Some(enm) =>
            Some(enumCompletion(context, enumSym, enm))
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
