/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of run-time tag checks for Unit.
  * - Elimination of run-time tag checks of singleton-valued enums.
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  * - Redundant branching (e.g. if(c1, if(c2, e2, e3), e3) -> if (c1 && c2, e2, e3))
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Simplified.fmtRoot(root).fmt(TerminalContext.AnsiTerminal))
    }

    // TODO: Implement.

    root.toSuccess
  }

}
