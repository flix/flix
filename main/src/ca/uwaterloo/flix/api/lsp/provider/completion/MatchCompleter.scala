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
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MatchCompletion
import ca.uwaterloo.flix.language.ast.{TypeConstructor, TypedAst}

object MatchCompleter {
  /**
    * Returns a List of Completion for match.
    */
  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[MatchCompletion] = {
    val matchPattern = raw".*\s*match(\s\S*)?".r

    if (!(matchPattern matches context.prefix)) {
      return Nil
    }

    val currentWordIsMatch = context.word == "match"

    root.enums.foldLeft[List[MatchCompletion]](Nil)((acc, enm) => {
      if (enm._2.cases.size >= 2) matchCompletion(enm._2, currentWordIsMatch) match {
        case Some(v) => v :: acc
        case None => acc
      }
      else acc
    })
  }

  /**
    * Converts an enum into an exhaustive match completion
    */
  private def matchCompletion(enm: TypedAst.Enum, currentWordIsMatch: Boolean): Option[MatchCompletion] = {
    val includeMatch = if (currentWordIsMatch) "match " else ""
    val (completion, _) = enm.cases.toList.sortBy(_._1.loc).foldLeft(("", 1))({
      case ((acc, z), (sym, cas)) =>
        val enumName = enm.sym.toString
        val caseName = sym.name
        val (str, k) = cas.tpe.typeConstructor match {
          case Some(TypeConstructor.Unit) => (s"$enumName.$caseName => $${${z + 1}:???}", z + 1)
          case Some(TypeConstructor.Tuple(arity)) => (List.range(1, arity + 1)
            .map(elem => s"$${${elem + z}:_elem$elem}")
            .mkString(s"$enumName.$caseName(", ", ", s") => $${${arity + z + 1}:???}"), z + arity + 1)
          case _ => (s"$enumName.$caseName($${${z + 1}:_elem}) => $${${z + 2}:???}", z + 2)
        }
        (acc + "    case " + str + "\n", k)
    })
    Some(MatchCompletion(enm, s"$includeMatch$${1:???} {\n$completion}"))
  }
}
