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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.VarCompletion
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.annotation.tailrec

/**
  * Ranks the possible var completions.
  */
object VarRanker {

  /**
    * Find the best var completion.
    *
    * @param completions the list of completions.
    * @param varUses     a map consisting of VarSym and its SourceLocations
    * @return            Some(Completion) if a better completion is possible, else none.
    */
  def findBest(completions: Iterable[Completion], varUses: MultiMap[Symbol.VarSym, SourceLocation]): Option[VarCompletion] = {
    /**
      * Auxiliary function for findBestVarCompletion.
      *
      * @param varComp  the var completion at question.
      * @param varComps the rest of possible var completions.
      * @param varUses  a multimap containing all uses of a specific var.
      * @return         Some(Completion) if a better completion is possible, else none.
      */
    @tailrec
    def aux(varComp: VarCompletion, varComps: Iterable[VarCompletion], varUses: MultiMap[Symbol.VarSym, SourceLocation]): Option[VarCompletion] = {
      // Checks how many times the var has been used
      varUses.apply(varComp.sym).size match {
        case 0 =>
          // The var hasn't been used yet, and is therefore the best var completion
          Some(varComp)
        case _ =>
          // The var has been used more at least once
          // Check if we have more varCompletions
          varComps match {
            case Nil =>
              // We don't have any more varCompletions, and therefore no best var completion
              None
            case ::(head, next) =>
              // We have more varCompletions, and we'll check if the next varCompletion is the best varCompletion
              aux(head, next, varUses)
          }
      }
    }

    // Remove all none var completions.
    getVarCompletions(completions) match {
      case Nil =>
        // Case 1: We don't have any possible var completions
        None
      case ::(head, next) =>
        // Case 2: We have some possible var completions
        aux(head, next, varUses)
    }
  }

  /**
    * Returns a list only consisting of var completions.
    *
    * @param completions the list of all possible completions.
    * @return            a List of VarCompletions.
    */
  private def getVarCompletions(completions: Iterable[Completion]): Iterable[VarCompletion] = {
    completions.flatMap {
      case VarCompletion(sym, tpe) => Some(VarCompletion(sym, tpe))
      case _ => None
    }
  }
}
