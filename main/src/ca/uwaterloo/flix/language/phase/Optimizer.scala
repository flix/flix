/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.{LiftedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.PrettyPrinter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Formatter, Validation}

/**
 * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner -> Reducer.
 */
object Optimizer {

  /**
   * Returns an optimized version of the given AST `root`.
   */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Optimizer") {
    var result = root

    for (_ <- 1 to 0) {
      val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(result)
      val afterInliner = Inliner.run(afterOccurrenceAnalyzer.get)
      val afterReducer = Reducer.run(afterInliner.get)
      result = afterReducer.get
    }

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Lifted.fmtRoot(result, Formatter.AnsiTerminalFormatter))
    }

    result.toSuccess
  }
}
