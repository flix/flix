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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TypeEnumCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.TypeCompleter.{formatTParams, formatTParamsSnippet, getInternalPriority, priorityBoostForTypes}
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object TypeEnumCompleter extends Completer {
  /**
    * Returns a List of Completion for enum types.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[TypeEnumCompletion] = {
    val word = context.word.split('.').toList
    val ns = word.dropRight(1)
    val currWord = word.takeRight(1).mkString

    println(s"word: $word")
    println(s"ns: $ns")
    println(s"currWord: $currWord")

    root.enums.flatMap {
      case (enmSym, enm) =>
        if (!enm.ann.isInternal && matchesTypeEnum(enmSym, currWord, ns)) {
          val internalPriority = getInternalPriority(enm.loc, enm.sym.namespace)(context)
          val name = enmSym.name
          Some(Completion.TypeEnumCompletion(enmSym, formatTParams(enm.tparams), priorityBoostForTypes(internalPriority(name))(context),
            TextEdit(context.range, s"$name${formatTParamsSnippet(enm.tparams)}"), Some(enm.doc.text)))
        } else {
          None
        }
    }
  }

  /**
    * Checks that the enumSym matches the word that the users is typing.
    *
    * @param sym  the enumSym.
    * @param word the current word.
    * @param ns   the provided namespace.
    * @return     true, if the enum matches word and namespace, false otherwise.
    */
  private def matchesTypeEnum(sym: Symbol.EnumSym, word: String, ns: List[String]): Boolean = {
    println(s"Sym.ns: ${sym.namespace}")
    println(s"Sym.name: ${sym.name}")
    val check = sym.namespace == ns && sym.name.startsWith(word)
    println(s"matchesTypeEnum: $check")
    check
  }
}
