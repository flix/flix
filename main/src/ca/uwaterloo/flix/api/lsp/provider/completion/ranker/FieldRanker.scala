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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion

object FieldRanker extends Ranker {

  /**
    * Find the best field completion.
    *
    * @param completions the list of completions.
    * @return            Some(FieldCompletion) if a better completion is possible, else none.
    */
  override def findBest(completions: Iterable[Completion])(implicit index: Index, deltaContext: DeltaContext): Option[FieldCompletion] = {
    // TODO
    None
  }
}
