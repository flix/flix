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
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
    context.word.split('.').toList match {
      case Nil => Nil
      case x :: Nil => getEnumTagCompletions(x, None)
      case x :: y :: Nil => getEnumTagCompletions(x, Some(y))
      case _ => Nil
    }
  }

  /**
    * Gets completions for enum tags
    */
  private def getEnumTagCompletions(enumName: String, tagName: Option[String])(implicit root: TypedAst.Root): Iterable[EnumTagCompletion] = {
    root.enums.filter {
      case (sym, _) => sym.name == enumName
    }.flatMap {
      case (sym, enm) =>
        enm.cases.flatMap {
          case (caseSym, _) =>
            tagName match {
              case None => Some(EnumTagCompletion(s"${sym.name}.${caseSym.name}"))
              case Some(tag) =>
                // This doesn't work...
                if (matchesTag(caseSym, tag))
                  Some(EnumTagCompletion(s"${sym.name}.${caseSym.name}"))
                else
                  None
            }
        }
    }
  }

  /**
    * Checks that the caseSym matches the prefix
    *
    * @param sym  the caseSym.
    * @param word the current prefix.
    * @return     true, if the var matches prefix, false otherwise.
    */
  private def matchesTag(sym: Symbol.CaseSym, word: String): Boolean = {
    word.nonEmpty && word.head.isUpper && sym.toString.startsWith(word)
  }
}
