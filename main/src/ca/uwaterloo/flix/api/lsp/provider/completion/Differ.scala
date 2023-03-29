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

object Differ {

  /**
    * Computes the semantic difference between the `oldAst` and `newAst`
    */
  def difference(old: Option[TypedAst.Root], newAst: TypedAst.Root): DeltaContext = old match {
    case None =>
      // Case 1: No old AST. No difference.
      DeltaContext(Nil)
    case Some(oldAst) =>
      // Case 2: We have an oldAst and a newAst. Compute their difference.

      // TODO: Add some comments and introduce helper functions.
      val newDefs = calcDefDiff(oldAst, newAst)
      val newEnums = (newAst.enums.keySet -- oldAst.enums.keySet).toList.map(sym => Delta.AddEnum(sym, getCurrentTimestamp))

      DeltaContext(newDefs ++ newEnums)
  }

  /**
    * Computes the difference between oldAst and newAst for defs
    *
    * @param oldAst the old AST.
    * @param newAst the new AST.
    * @return a list of changes as Delta's.
    */
  private def calcDefDiff(oldAst: TypedAst.Root, newAst: TypedAst.Root): List[Delta] = {
    newAst.defs.flatMap {
      case (sym, newDef) =>
        // Check if the sym also exists in the oldAst
        oldAst.defs.get(sym) match {
          case None =>
            // It's a newly added def, add it to the deltas
            Some(Delta.AddDef(sym, getCurrentTimestamp))
          case Some(oldDef) =>
            // We have an old def, check if the implementation has changed
            if (compareDefs(oldDef, newDef)) {
              // The def has changed, add it to the deltas
              Some(Delta.AddDef(sym, getCurrentTimestamp))
            } else {
              // The def hasn't changed, don't add it to the deltas.
              None
            }
        }
    }.toList
  }

  /**
    * Compares two defs in terms of change
    *
    * @param oldDef the old def.
    * @param newDef the new def.
    * @return true if the def has changed, false otherwise.
    */
  private def compareDefs(oldDef: TypedAst.Def, newDef: TypedAst.Def): Boolean = {
    oldDef.impl.exp.loc.beginCol != newDef.impl.exp.loc.beginCol ||
      oldDef.impl.exp.loc.endCol != newDef.impl.exp.loc.endCol ||
      oldDef.impl.exp.loc.beginLine - oldDef.impl.exp.loc.endLine != newDef.impl.exp.loc.beginLine - newDef.impl.exp.loc.endLine
  }

  /**
    * Returns the current timestamp.
    */
  private def getCurrentTimestamp: Long = System.nanoTime()

}
