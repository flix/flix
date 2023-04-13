/*
 * Copyright 2023 Magnus Madsen, Lukas Rønn
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

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.Symbol

object Differ {

  /**
    * Computes the semantic difference between the `oldAst` and `newAst`
    */
  def difference(currDelta: DeltaContext, old: Option[TypedAst.Root], newAst: TypedAst.Root): DeltaContext = old match {
    case None =>
      // Case 1: No old AST. No difference.
      currDelta
    case Some(oldAst) =>
      // Case 2: We have an oldAst and a newAst. Compute their difference.

      // TODO: Add some comments and introduce helper functions.
      val newDefs = findModifiedDefs(oldAst, newAst)

      DeltaContext(Delta(newDefs))
  }

  private def findModifiedDefs(oldAst: TypedAst.Root, newAst: TypedAst.Root): Map[Symbol.DefnSym, Long] = {
    Map.empty
  }


  /**
    * Returns the current timestamp.
    */
  private def getCurrentTimestamp: Long = System.nanoTime()

}
