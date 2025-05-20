/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

object Optimizer {

  /**
    * The maximum number of rounds to run the inliner for.
    */
  private val MaxRounds: Int = 5

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    var currentRoot = root
    var currentDelta = currentRoot.defs.keys.toSet
    var currentLive = Set.empty[Symbol.DefnSym]
    var depGraph = List.empty[List[Symbol.DefnSym]]
    var hasComputedDepGraphOnce = false
    for (_ <- 0 until MaxRounds) {
      if (currentDelta.nonEmpty) {
        val (afterOccurrenceAnalyzer, graph) = OccurrenceAnalyzer.run(currentRoot, currentDelta, !hasComputedDepGraphOnce)
        if (!hasComputedDepGraphOnce) {
          hasComputedDepGraphOnce = true
          depGraph = graph match {
            case Nil =>
              List.empty

            case _ :: next =>
              // Skip leaf group
              next
          }
        }
        val depGroup = depGraph match {
          case Nil => List.empty
          case group :: next =>
            depGraph = next
            group
        }
        val (newRoot, newDelta, newLive) = Inliner.run(afterOccurrenceAnalyzer, depGroup)
        currentRoot = newRoot
        currentDelta = newDelta
        currentLive ++= newLive
      }
    }
    // val liveDefs = currentRoot.defs.filter { case (sym, _) => currentLive.contains(sym) }
    // currentRoot.copy(defs = liveDefs)
    // TODO: Figure out online tree shaking. Maybe return SCCs from OA and always include those.
    currentRoot
  }

}
