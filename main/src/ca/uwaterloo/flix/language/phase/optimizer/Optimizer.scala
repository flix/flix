/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    var currentRoot = root
    var currentDelta = currentRoot.defs.keys.toSet
    for (_ <- 0 until CompilerConstants.MaxOptimizerRounds) {
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
