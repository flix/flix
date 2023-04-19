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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.DefCompletion

object DefRanker {

  /**
    * Find the best/newest def completion.
    *
    * Checks each def completion if that def has been modified, which is done by a lookup in the delta map.
    *
    * The def completion with the most recent timeStamp will be returned, as the bestDefCompletion.
    *
    * @param completions  the list of completions.
    * @param deltaContext the current changes
    * @return             Some(DefCompletion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion], deltaContext: DeltaContext): Option[DefCompletion] = {
    getDefCompletions(completions).maxByOption(defComp => deltaContext.defs.get(defComp.decl.sym))
  }

  /**
    * Removes all none Def completions from the list of all possible completions.
    *
    * @param comps the list of all possible completions.
    * @return a List[DefCompletion] only consisting of Def completions.
    */
  private def getDefCompletions(comps: Iterable[Completion]): Iterable[DefCompletion] = {
    comps.collect {
      case DefCompletion(decl) => DefCompletion(decl)
    }
  }
}
