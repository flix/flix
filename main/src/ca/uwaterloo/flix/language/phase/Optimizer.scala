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
import ca.uwaterloo.flix.language.ast.LiftedAst.Root
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Expression
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

    for (_ <- 1 to 2) {
      val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(result)
      val afterInliner = Inliner.run(afterOccurrenceAnalyzer.get)
      result = afterInliner.get
    }

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Lifted.fmtRoot(result, Formatter.AnsiTerminalFormatter))
    }

    result.toSuccess
  }

  /**
   * returns `true` if `exp0` is considered a trivial expression.
   *
   * An expression is trivial if:
   * It is either a literal (float, string, int, bool, unit), or it is a variable.
   *
   * A pure and trivial expression can always be inlined even without duplicating work.
   */
  def isTrivialExp(exp0: Expression): Boolean = exp0 match {
    case Expression.Unit(_) => true
    case Expression.Null(_, _) => true
    case Expression.True(_) => true
    case Expression.False(_) => true
    case Expression.Char(_, _) => true
    case Expression.Float32(_, _) => true
    case Expression.Float64(_, _) => true
    case Expression.Int8(_, _) => true
    case Expression.Int16(_, _) => true
    case Expression.Int32(_, _) => true
    case Expression.Int64(_, _) => true
    case Expression.BigInt(_, _) => true
    case Expression.Str(_, _) => true
    case Expression.Var(_, _, _) => true
    case _ => false
  }
}
