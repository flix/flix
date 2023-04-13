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

/**
  * Represents a list of changes.
  */
case class DeltaContext(delta: Delta)

/**
  * Represents changes between ASTs.
  */
object DeltaContext {
  /**
    * Merges two DeltaContext.
    *
    * @param d1 the first DeltaContext.
    * @param d2 the second DeltaContext.
    * @return   a DeltaContext with a new Delta containing all changes
    */
  def mergeDeltas(d1: DeltaContext, d2: DeltaContext): DeltaContext = {
    DeltaContext(Delta(d1.delta.defs ++ d2.delta.defs))
  }
}
