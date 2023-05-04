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
    //
    // We match on either `A.B.C` or `A.B.C.` In the latter case we have to remove the dot.
    //
    val fqn = if (context.word.last == '.') context.word.dropRight(1) else context.word

    val enumSym = Symbol.mkEnumSym(fqn)

    root.enums.get(enumSym) match {
      case None => // Case 1: Enum does not exist.
        Nil
      case Some(enm) =>
      // Case 2: Enum found, get the cases:
      enm.cases.map {
        case (caseSym, _) => EnumTagCompletion(enumSym, caseSym)
      }
    }
  }
}
