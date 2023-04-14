/*
 * Copyright 2023 Lukas Rønn, Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * Represents a list of changes.
  */
case class DeltaContext(defs: Map[Symbol.DefnSym, Long])

/**
  * Represents changes between ASTs.
  */
object DeltaContext {
  /**
    * Merges two DeltaContext.
    *
    * This is not a commutative operation.
    *
    * If d1 and d2 contains the same key, but a different value, the resulting DeltaContext
    * will contain a mapping from that key to the value from d2.
    *
    * @param d1 the first DeltaContext.
    * @param d2 the second DeltaContext.
    * @return   a DeltaContext with a new map containing all changes.
    */
  def mergeDeltas(d1: DeltaContext, d2: DeltaContext): DeltaContext = {
    DeltaContext(d1.defs ++ d2.defs)
  }
}
