package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    var result = root
    var delta = result.defs.keys.toSet
    for (_ <- 1 to 3) {
      val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(result, delta)
      val (inlinerRoot, inlinerChange) = Inliner.run(afterOccurrenceAnalyzer)
      result = inlinerRoot
      delta = inlinerChange
    }
    result
  }
}
