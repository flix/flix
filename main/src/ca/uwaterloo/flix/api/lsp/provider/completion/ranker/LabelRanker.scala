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
import ca.uwaterloo.flix.api.lsp.provider.completion.{Completion, CompletionContext, DeltaContext}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.LabelCompletion

object LabelRanker extends Ranker {

  /**
    * Find the best [[LabelCompletion]].
    *
    * @param completions the list of completions.
    * @return Some([[LabelCompletion]]) if a better completion is possible, else `None`.
    */
  override def findBest(completions: Iterable[Completion])(implicit context: CompletionContext, index: Index, deltaContext: DeltaContext): Option[LabelCompletion] = {
    // Remove all none label completions
    getLabelCompletions(completions)
      // Find the label comp that has 0 uses
      .find(labelComp => index.labelUses(labelComp.label).isEmpty)
  }

  /**
    * Returns a list only consisting of [[LabelCompletion]]s.
    *
    * @param completions the list of all possible completions.
    * @return a list of [[LabelCompletion]]s.
    */
  private def getLabelCompletions(completions: Iterable[Completion]): Iterable[LabelCompletion] = {
    completions.collect {
      case comp: LabelCompletion => comp
    }
  }
}
