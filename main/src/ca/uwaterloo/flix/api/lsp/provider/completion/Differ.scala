/*
 * Copyright 2023 Magnus Madsen
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

object Differ {

  /**
    * Computes the semantic difference between the `oldAst` and `newAst`
    */
  def difference(old: Option[TypedAst.Root], newAst: TypedAst.Root): DeltaContext = old match {
    case None =>
      // Case 1: No old AST. No difference.
      DeltaContext(Nil)
    case Some(oldAst) => {
      // Case 2: We have an oldAst and a newAst. Compute their difference.

      // TODO: Add some comments and introduce helper functions.
      val newDefs = (newAst.defs.keySet -- oldAst.defs.keySet).toList.map(sym => Delta.AddDef(sym, getCurrentTimestamp))
      val newEnums = (newAst.enums.keySet -- oldAst.enums.keySet).toList.map(sym => Delta.AddEnum(sym, getCurrentTimestamp))

      DeltaContext(newDefs ++ newEnums)
    }
  }

  /**
    * Returns the current timestamp.
    */
  private def getCurrentTimestamp: Long = System.nanoTime()

}
