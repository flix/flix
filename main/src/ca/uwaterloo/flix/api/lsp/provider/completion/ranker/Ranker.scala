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
import ca.uwaterloo.flix.api.lsp.provider.completion.{Completion, DeltaContext}

/**
  * Ranker
  *
  * Ranks the completions to better suit the actual needs for the user.
  */
trait Ranker {
  /**
    * Calculate the best 1st completion in popup-pane
    *
    * @param completions the list of decided completions.
    * @return            Some(Completion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion])(implicit index: Index, deltaContext: DeltaContext): Option[Completion]
}
