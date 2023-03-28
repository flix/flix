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
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * CompletionRanker
  *
  * Ranks the completions to better suit the actual needs for the user.
  */
object CompletionRanker {

  /**
    * Calculate the best 1st completion in popup-pane
    *
    * @param completions       the list of decided completions.
    * @param deltaContext      current list of delta's.
    * @return Some(Completion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion], deltaContext: DeltaContext): Option[Completion] =
    deltaContext.deltas match {
      case Nil => None
      case deltas =>
        findBestDefCompletion(completions, deltas)
    }

  /**
    * Find the best/newest def completion
    *
    * @param completions  the list of completions.
    * @param deltas       the list of changes.
    * @return             Some(DefCompletion) if a better completion is possible, else none.
    */
  private def findBestDefCompletion(completions: Iterable[Completion], deltas: List[Delta]): Option[Completion] = {
    val (bestDefComp, _) =
      // Iterate through all deltas and find the defs
      deltas.foldLeft[(Option[Completion], Long)](None, 0) {
        case ((currBestDef, currBestTimestamp), delta) =>
          delta match {
            case Delta.AddDef(sym, timestamp) =>
              // We have a def, find that specific def in the list of completions
              completions.find(comp => comp match {
                case DefCompletion(decl) =>
                  // Check if the def found is the same as the def from delta and check if it has the most recent timestamp
                  isAChangedDef(decl, sym) && isABetterDefCompletion(currBestTimestamp, timestamp)
                case _ =>
                  // Not a def
                  false
              }) match {
                case None =>
                  // We don't have a newer def
                  (currBestDef, currBestTimestamp)
                case comp =>
                  // We have a newer added/changed def
                  (comp, timestamp)
              }
            case _ =>
              // Not a def
              (currBestDef, currBestTimestamp)
          }
        }
    bestDefComp
  }

  /**
    * Checks if the def decl is changed/added.
    *
    * @param decl        the def decl.
    * @param deltaDefSym the sym of a changed def decl.
    * @return            true, if the def decl at question is changed, false otherwise.
    */
  private def isAChangedDef(decl: TypedAst.Def, deltaDefSym: DefnSym): Boolean = {
    decl.sym == deltaDefSym
  }

  /**
    * Checks if the timestamp for the changed/added def is better than the current best.
    *
    * @param timestamp          the timestamp of the def in the List[Delta]
    * @param currBestTimestamp  the timestamp of the current best def completion.
    * @return                   true if the def has a newer timestamp than the currBest, false otherwise.
    */
  private def isABetterDefCompletion(currBestTimestamp: Long, timestamp: Long): Boolean = {
    currBestTimestamp < timestamp
  }
}
