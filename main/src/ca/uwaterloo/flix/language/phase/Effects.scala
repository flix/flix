/*
 * Copyright 2018 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.EffectError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

/**
  * A phase that computes the effect of every expression in the program.
  */
object Effects extends Phase[Root, Root] {

  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, EffectError] = flix.phase("Effects") {

    // TODO: Implement Effects.

    // TODO: Need not only effects, but also whether a result must be used or not.

    return root.toSuccess

  }
}
