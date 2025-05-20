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
import ca.uwaterloo.flix.language.ast.{MonoAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.InternalCompilerException

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
    val (afterOccurrenceAnalyzer0, graph, sccs) = OccurrenceAnalyzer.run(currentRoot, currentDelta, computeDependencyGraph = true)
    var afterOccurrenceAnalyzer = afterOccurrenceAnalyzer0
    var depGraph = graph match {
      case Nil =>
        List.empty

      case _ :: next =>
        // Skip leaf group
        next
    }
    while (depGraph.nonEmpty) {
      depGraph match {
        case Nil =>
          throw InternalCompilerException("unexpected empty dependency graph", SourceLocation.Unknown)

        case group :: Nil =>
          depGraph = List.empty
          val (newRoot, newDelta, newLive) = Inliner.run(afterOccurrenceAnalyzer, group, sccs)
          currentRoot = newRoot
          currentDelta = newDelta
          currentLive ++= newLive

        case group :: rest =>
          depGraph = rest
          val (newRoot, newDelta, newLive) = Inliner.run(afterOccurrenceAnalyzer, group, sccs)
          currentRoot = newRoot
          currentDelta = newDelta
          currentLive ++= newLive
          val (afterOccurrenceAnalyzer1, _, _) = OccurrenceAnalyzer.run(currentRoot, currentDelta, computeDependencyGraph = false)
          afterOccurrenceAnalyzer = afterOccurrenceAnalyzer1
      }
    }
    val liveDefs = currentRoot.defs.filter {
      case (sym, _) =>
        // Case 1: A def is live because it was not inlined.
        // Case 4: It is an entrypoint so always include it.
        currentLive.contains(sym) || currentRoot.entryPoints.contains(sym) || sccs.contains(sym)
    }
    currentRoot.copy(defs = liveDefs)
  }
}
