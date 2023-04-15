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

import scala.annotation.tailrec

object DefRanker {

  /**
    * Find the best/newest def completion.
    *
    * @param completions  the list of completions.
    * @param deltaContext the current changes
    * @return             Some(DefCompletion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion], deltaContext: DeltaContext): Option[DefCompletion] = {
    /**
      * Auxiliary function for findBestDefCompletion.
      *
      * It iterates through the list of all possible def completions.
      * For each def completion it checks if that def has been modified, which is done by a lookup in the delta map.
      *
      * Stores the current best completion and its timestamp until all def completions has been checked.
      *
      * The def completion with the most recent timeStamp will be returned, as the bestDefCompletion.
      *
      * @param defComp           the def completion at question.
      * @param defComps          the rest of the def completions.
      * @param currBestDef       the current best def completion.
      * @param currBestTimestamp the associated timeStamp of the current best def completion.
      * @return                  Some(DefCompletion) if a better completion is possible, else none.
      */
    @tailrec
    def aux(defComp: DefCompletion, defComps: Iterable[DefCompletion], currBestDef: Option[DefCompletion], currBestTimestamp: Long): Option[DefCompletion] = {
      val (bestDef, bestTimeStamp) =
      // Check if the delta contains a modification of the def at question, and get the timeStamp
        deltaContext.defs.get(defComp.decl.sym) match {
          case None =>
            // The def hasn't been modified
            // Our currBest is still our current best def completion
            (currBestDef, currBestTimestamp)
          case Some(timeStamp) =>
            // The def has been modified
            // Check if it has a newer timestamp than the current best
            if (currBestTimestamp < timeStamp) {
              // We have a newer def
              (Some(defComp), timeStamp)
            } else {
              // We don't have a newer timeStamp than the current best
              (currBestDef, currBestTimestamp)
            }
        }
      // Check if we should search for a possible better def completion
      defComps match {
        case Nil =>
          // We don't have any more defs, so the one we found is the best
          bestDef
        case ::(head, next) =>
          // We have more defs, continue search for the best
          aux(head, next, bestDef, bestTimeStamp)
      }
    }

    // Remove all none Def completions
    getDefCompletions(completions) match {
      case Nil =>
        // We don't have any Def completions and therefore no best def completion
        None
      case ::(head, next) =>
        // We have some Def completions, find the best
        aux(head, next, None, 0)
    }
  }

  /**
    * Removes all none Def completions from the list of all possible completions.
    *
    * @param comps the list of all possible completions.
    * @return a List[DefCompletion] only consisting of Def completions.
    */
  private def getDefCompletions(comps: Iterable[Completion]): Iterable[DefCompletion] = {
    comps.flatMap {
      case DefCompletion(decl) => Some(DefCompletion(decl))
      case _ => None
    }
  }
}
