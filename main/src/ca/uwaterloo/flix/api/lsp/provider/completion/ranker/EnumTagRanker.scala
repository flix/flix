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

package ca.uwaterloo.flix.api.lsp.provider.completion.ranker

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.EnumTagCompletion
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.MultiMap
import ca.uwaterloo.flix.api.lsp.provider.completion.ranker.CompletionRanker.hasRealSourceKinds

object EnumTagRanker {

  /**
    * Find the best enum tag completion.
    */
  def findBest(completions: Iterable[Completion], tagUses: MultiMap[Symbol.CaseSym, SourceLocation]): Option[EnumTagCompletion] = {
    // Remove all none typeEnum completions
    getEnumTagCompletions(completions)
      // Find the typeEnum comp that has 0 Real uses
      .find(enumTag =>
        !hasRealSourceKinds(tagUses(enumTag.caseSym)))
  }

  /**
    * Returns a list only consisting of enumTag completions.
    *
    * @param completions the list of all possible completions.
    * @return            a List of EnumTagCompletion.
    */
  private def getEnumTagCompletions(completions: Iterable[Completion]): Iterable[EnumTagCompletion] = {
    completions.collect {
      case comp: EnumTagCompletion => comp
    }
  }
}
