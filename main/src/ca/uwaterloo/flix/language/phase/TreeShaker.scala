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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Root

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) Appears in the global namespaces, takes zero arguments, and is not marked as synthetic.
  * (b) Appears in a fact or a rule as a filter/transfer function.
  * (c) Appears in a function which itself is reachable.
  */
object TreeShaker {

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def shake(root: Root)(implicit genSyn: GenSym): Root = {

    // TODO: Implement.

    root
  }

}
