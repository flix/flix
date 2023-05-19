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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MatchCompletion

object MatchRanker extends Ranker {
  /**
    * Find the best match completion.
    *
    * @param completions the list of decided completions.
    * @return            Some(MatchCompletion) if a better completion is possible, else none.
    */
  override def findBest(completions: Iterable[Completion])(implicit context: CompletionContext, index: Index, deltaContext: DeltaContext): Option[MatchCompletion] = {
    // Remove all none match completions
    getMatchCompletions(completions)
      // Filter all match comps where enm is public, is in context.uri and has empty nameSpace
      .filter(matchComp => matchComp.enm.mod.isPublic && matchComp.enm.loc.source.name == context.uri && matchComp.enm.sym.namespace.isEmpty)
      // Find the one with shortest distance to the users position
      .minByOption(matchComp => math.abs(context.pos.line - matchComp.enm.loc.beginLine))
  }

  /**
    * Returns a list only consisting of match completions.
    *
    * @param completions the list of all possible completions.
    * @return            a List of MatchCompletion.
    */
  private def getMatchCompletions(completions: Iterable[Completion]): Iterable[MatchCompletion] = {
    completions.collect {
      case comp: MatchCompletion => comp
    }
  }
}
