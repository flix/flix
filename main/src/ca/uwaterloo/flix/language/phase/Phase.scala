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
import ca.uwaterloo.flix.util.Validation

/**
  * An interface for a compiler phase.
  *
  * @tparam I the input type.
  * @tparam O the output type.
  */
trait Phase[I, O] {

  /**
    * Runs the phase.
    */
  def run(input: I)(implicit flix: Flix): Validation[O, CompilationError]

  /**
    * Returns a phase that is the result of applying `this` phase followed by `that` phase.
    */
  def |>[O2](that: Phase[O, O2]): Phase[I, O2] = new Phase[I, O2] {
    def run(input: I)(implicit flix: Flix): Validation[O2, CompilationError] =
      Phase.this.run(input) flatMap {
        case output => that.run(output)
      }
  }

}
