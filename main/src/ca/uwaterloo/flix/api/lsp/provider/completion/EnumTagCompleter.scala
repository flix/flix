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
    context.word.split('.').toList match {
      /* We have the following cases:
         0: We have nothing
         1: We have path or enum
         2: We have a path.enum, enum.tag, path.path
            2.1: We have A.Color
            2.2: We have Color.Green
            2.3: We have A.B
         3: We have an path.path.enum or path.enum.tag
            3.1: We have A.B.Color
            3.2: We have A.Color.Green
         4: We have path.enum.tag i.e. A.B.Color.Green
       */

      // Case 0:
      case Nil => Nil
      // Case 1:
      case x :: Nil =>
        getEnumTagCompletions(Nil, x, None)
      // Case 2:
      case x :: y :: Nil =>
        val ns1 = List(x)
        // Case 2.1:
        getEnumTagCompletions(ns1, y, None) ++
          // Case 2.2:
          getEnumTagCompletions(Nil, x, Some(y))
          // Case 2.3:
          // Do nothing, not valid for tags
      // Case 3:
      case x :: y :: z :: Nil =>
        val ns1 = List(x)
        val ns2 = List(x, y)
        // Case 3.1:
        getEnumTagCompletions(ns2, z, None) ++
          // Case 3.2
          getEnumTagCompletions(ns1, y, Some(z))
      // Case 4:
      case ns1 :: ns2 :: enum :: tag :: Nil =>
        val ns = List(ns1, ns2)
        getEnumTagCompletions(ns, enum, Some(tag))
      case _ => Nil
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
    * It's allowed not to provide a namespace, hence the user could have a 'use A.B.Color2;'.
    * The user should therefore not be required to provide the path.
    *
    * @param ns  the provided nameSpace
    * @param sym the enumSym
    * @return    true, if the ns.isEmpty or it matches sym.namespace, false otherwise.
    */
  private def providedNameSpaceIsValid(ns: List[String], sym: Symbol.EnumSym): Boolean = {
    ns.isEmpty || ns == sym.namespace
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
