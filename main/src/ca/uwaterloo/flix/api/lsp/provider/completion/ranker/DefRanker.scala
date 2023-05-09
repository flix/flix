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

import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.DefCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.{Completion, DeltaContext}

object DefRanker extends Ranker {

  /**
    * Find the best/newest def completion.
    *
    * Checks each def completion if that def has been modified, which is done by a lookup in the delta map.
    *
    * The def completion with the most recent timeStamp will be returned, as the best DefCompletion.
    *
    * @param completions  the list of completions.
    * @return             Some(DefCompletion) if a better completion is possible, else none.
    */
  override def findBest(completions: Iterable[Completion])(implicit index: Index, deltaContext: DeltaContext): Option[DefCompletion] = {
    getDefCompletions(completions)
      // If the map does not contain any of the keys, it unintentionally returns the first possible def completion.
      // The filter makes sure, that this does not happen.
      .filter(defComp => deltaContext.defs.contains(defComp.decl.sym))
      .maxByOption(defComp => deltaContext.defs.get(defComp.decl.sym))
  }

  /**
    * Removes all none Def completions from the list of all possible completions.
    *
    * @param comps the list of all possible completions.
    * @return      a List[DefCompletion] only consisting of Def completions.
    */
  private def getDefCompletions(comps: Iterable[Completion]): Iterable[DefCompletion] = {
    comps.collect {
      case defComp: DefCompletion => defComp
    }
  }
}
