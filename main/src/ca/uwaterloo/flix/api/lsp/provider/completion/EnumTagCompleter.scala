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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumTagCompletion
import ca.uwaterloo.flix.language.ast.{TypedAst, Symbol}

object EnumTagCompleter extends Completer {

  /**
    * Returns a List of Completion for enum tags.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[EnumTagCompletion] = {
    val word = context.word.split('.').toList
    /* We have the following cases:
             0: We have nothing
             1: We have path or enum
                1.1: We have A
                1.2: We have Color
             2: We have a path.enum, enum.tag, path.path
                2.1: We have A.Color
                2.2: We have Color.Green
                2.3: We have A.B
             3: We have an a longer path.enum or longer path.enum.tag
                3.1: We have A.X.B.Color
                3.2: We have A.X.Color.Green
           */
    word.length match {
      // Case 0:
      case 0 => Nil
      // Case 1:
      case 1 =>
        // Case 1.1:
        // Do nothing, not valid for tags
        // Case 1.2:
        val enm = word.head
        getEnumTagCompletions(Nil, enm, None)
      // Case 2:
      case 2 =>
        val ns = word.take(1)
        val enmOrTag = word(1)
        val enm2 = word.head
        // Case 2.1:
        getEnumTagCompletions(ns, enmOrTag, None) ++
          // Case 2.2:
          getEnumTagCompletions(Nil, enm2, Some(enmOrTag))
          // Case 2.3:
          // Do nothing, not valid for tags
      // Case 3:
      case x =>
        val ns1 = word.take(x - 1)
        val ns2 = word.take(x - 2)
        val enmOrTag = word(x - 1)
        val enm2 = word(x - 2)
        // Case 3.1:
        getEnumTagCompletions(ns1, enmOrTag, None) ++
          // Case 3.2
          getEnumTagCompletions(ns2, enm2, Some(enmOrTag))

    }
  }

  /**
    * Gets completions for enum tags
    */
  private def getEnumTagCompletions(ns: List[String], enumName: String, tagName: Option[String])(implicit root: TypedAst.Root): Iterable[EnumTagCompletion] = {
    root.enums.filter {
      case (enumSym, _) => enumSym.name == enumName && providedNameSpaceIsValid(ns, enumSym)
    }.flatMap {
      case (enumSym, enm) =>
        enm.cases.flatMap {
          case (caseSym, _) =>
            tagName match {
              case None => Some(EnumTagCompletion(enumSym, caseSym, ns))
              case Some(tag) =>
                if (matchesTag(caseSym, tag)) {
                  Some(EnumTagCompletion(enumSym, caseSym, ns))
                } else {
                  None
                }
            }
        }
    }
  }

  /**
    * This checks if the provided nameSpace from the user matches the nameSpace for the Enum.
    *
    * It's allowed not to provide a namespace, hence the user could have a 'use A.B.Color'.
    * The user should therefore not be required to provide the path.
    *
    * Further if the user has 'use A.B', it is only required to provide 'B.Color'.
    *
    * @param ns  the provided nameSpace.
    * @param sym the enumSym.
    * @return    true, if the ns.isEmpty or it matches sym.namespace, false otherwise.
    */
  private def providedNameSpaceIsValid(ns: List[String], sym: Symbol.EnumSym): Boolean = {
    ns.isEmpty || ns == sym.namespace || ns == sym.namespace.reverse.take(ns.length - 1)
  }

  /**
    * Checks that the caseSym matches the prefix
    *
    * @param sym  the caseSym.
    * @param word the current prefix.
    * @return     true, if the var matches prefix, false otherwise.
    */
  private def matchesTag(sym: Symbol.CaseSym, word: String): Boolean =
    word.nonEmpty && word.head.isUpper && sym.name.startsWith(word)
}
