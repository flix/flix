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

import scala.annotation.tailrec

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
    /**
      * Auxiliary function for findBestDefCompletion
      *
      * @param completions  the list of possible completions.
      * @param delta        the modifiedDefDelta at question.
      * @param deltas       the rest of the modifiedDefDeltas as a list.
      * @return             Some(DefCompletion) if a better completion is possible, else none.
      */
    @tailrec
    def aux(completions: Iterable[Completion], delta: Delta.ModifiedDef, deltas: List[Delta.ModifiedDef]): Option[Completion] = {
      // We have a def, find that specific def in the list of completions
      completions.find(comp => comp match {
        case DefCompletion(decl) =>
          // Check if the def found is the same as the def from delta
          isAModifiedDef(decl, delta.sym)
        case _ =>
          // Not a def
          false
      }) match {
        case None =>
          // This delta isn't part of the possible completions
          // Check if there are more deltas
          if (deltas.isEmpty) {
            // No more deltas. We don't have a best def completion
            None
          } else {
            // More deltas, check if the next delta is part of the possible completions
            aux(completions, deltas.head, deltas.tail)
          }
        case comp =>
          // The delta is part of the completions, return that completion.
          comp
      }
    }

    // Check if we have a List of deltas
    deltas match {
      case Nil => None
      case deltasForFilter =>
        // Remove all none ModifiedDef deltas, and reverse the list, so the most recent defDelta is the first elem.
        val defDeltas: List[Delta.ModifiedDef] = deltasForFilter.flatMap(delta => delta match {
          case Delta.ModifiedDef(sym, timestamp) => Some(Delta.ModifiedDef(sym, timestamp))
          case _ => None
        }).reverse
        if (defDeltas.isEmpty) {
            // We don't have any defDeltas and therefore no best def completion
            None
        } else {
            // We have some defDeltas
            aux(completions, defDeltas.head, defDeltas.tail)
        }
    }
  }

  /**
    * Checks if the def decl is modified. Hence the sym from delta is the same is the one in the completion.
    *
    * @param decl        the def decl.
    * @param deltaDefSym the sym of a modified def decl.
    * @return            true, if the def decl at question is changed, false otherwise.
    */
  private def isAModifiedDef(decl: TypedAst.Def, deltaDefSym: DefnSym): Boolean = {
    decl.sym == deltaDefSym
  }
}
