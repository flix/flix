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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.TypeEnumCompletion
import ca.uwaterloo.flix.language.ast.{SourceKind, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.MultiMap

object TypeEnumRanker {

  /**
    * Find the best type enum completion.
    *
    * @param completions the list of completions.
    * @param enumUses    a map consisting of EnumSym and its SourceLocations
    * @return            Some(TypeEnumCompletion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion], enumUses: MultiMap[Symbol.EnumSym, SourceLocation]): Option[TypeEnumCompletion] = {
    // Remove all none typeEnum completions
    getTypeEnumCompletions(completions)
      // Find the typeEnum comp that has 0 Real uses
      .find(typeEnumComp =>
        !hasRealSourceKinds(enumUses(typeEnumComp.enumSym)))
  }

  /**
    * Returns a list only consisting of typeEnum completions.
    *
    * @param  completions the list of all possible completions.
    * @return a List of TypeEnumCompletion.
    */
  private def getTypeEnumCompletions(completions: Iterable[Completion]): Iterable[TypeEnumCompletion] = {
    completions.collect {
      case comp: TypeEnumCompletion => comp
    }
  }

  /**
    * Checks if set[SourceLocation] contain any SourceKind.Real
    *
    * @param  set the set of SourceLocations.
    * @return true, if at least one of the SourceKinds are Real, false otherwise.
    */
  private def hasRealSourceKinds(set: Set[SourceLocation]): Boolean =
    set.exists(_.locationKind == SourceKind.Real)
}
