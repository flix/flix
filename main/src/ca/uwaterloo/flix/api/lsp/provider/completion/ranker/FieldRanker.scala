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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FieldCompletion

object FieldRanker extends Ranker {

  /**
    * Find the best field completion.
    *
    * @param completions the list of completions.
    * @return            Some(FieldCompletion) if a better completion is possible, else none.
    */
  override def findBest(completions: Iterable[Completion])(implicit context: CompletionContext, index: Index, deltaContext: DeltaContext): Option[FieldCompletion] = {
    // Remove all none field completions
    getFieldCompletions(completions)
      // Find the field comp that has 0 uses
      .find(fieldComp => index.fieldUses(fieldComp.field).isEmpty)
  }

  /**
    * Returns a list only consisting of field completions.
    *
    * @param completions the list of all possible completions.
    * @return            a List of FieldCompletions.
    */
  private def getFieldCompletions(completions: Iterable[Completion]): Iterable[FieldCompletion] = {
    completions.collect {
      case comp: FieldCompletion => comp
    }
  }
}
