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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Root

/**
  * The inlining phase performs careful inlining of select functions based on heuristics.
  *
  * - A function of zero arguments is always inlined.
  * - A function of one boolean argument is always inlined.
  * - A function of two boolean arguments is always inlined.
  * - A function with a small "heuristic score" is always inlined.
  */
object Inliner {

  /**
    * The maximum score of a function for it to be eligible for inlining.
    */
  val MaxScore = 25

  /**
    * Performs inlining on the given AST `root`.
    */
  def inline(root: Root)(implicit genSyn: GenSym): Root = {

    // TODO: Implement.

    root
  }

  /**
    * Returns the score of the given function definition `defn`.
    *
    * The score of an expression is computed as:
    *
    * - Every expression is worth one point (plus the sum of its children), except:
    * - A literal is worth zero points.
    * - A UserError, MatchError, or SwitchError is worth zero points.
    * - An if-then-else statement is worth the value of the condition plus two times the sum of the consequent and alternative.
    */
  def score(defn: Constant): Boolean = ??? // TODO

}
