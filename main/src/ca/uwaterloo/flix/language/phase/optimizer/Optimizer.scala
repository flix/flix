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
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    var currentRoot = root
    var currentDelta = currentRoot.defs.keys.toSet
    for (_ <- 0 until 5) {
      if (currentDelta.nonEmpty) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(currentRoot, currentDelta)
        val (newRoot, newDelta) = Inliner.run(afterOccurrenceAnalyzer)
        currentRoot = newRoot
        currentDelta = newDelta
      }
    }
    currentRoot
  }

}
