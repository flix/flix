/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The stratification phase computes the strongly-connected components (SCCs)
  * of the constraints and computes a topological sort of the SCCs.
  *
  * Reports a [[StratificationError]] if the constraint graph contains negative cycles.
  */
object Stratifier {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def stratify(root: Root): Validation[Root, StratificationError] = {

    // TODO: Implement

    root.toSuccess
  }

}
