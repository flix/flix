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
    /*
      The code below seems very cumbersome. I would propose:

        1. Compute all symbols in both ASTs.
        2. Iterate through this set and call compareDefs on each pair (if both exist).

      Putting this into some helper functions.
     */


    // Find all newly added defs
    val newlyAddedDefs = (newAst.defs.keySet -- oldAst.defs.keySet).toList.map(sym => Delta.AddDef(sym, getCurrentTimestamp))
    // Check if any of the oldDefs has changed implementation
    val changedDefs =
      newAst.defs.keySet.toList.flatMap(sym =>
        oldAst.defs.get(sym) match {
          case Some(oldDef) =>
            newAst.defs.get(sym) match {
              case Some(newDef) =>
                // Case 1: We have the same sym in both new and old AST. Check if the implementation has changed
                if (compareDefs(oldDef, newDef)) {
                  // Decl has changed
                  Some(Delta.AddDef(sym, getCurrentTimestamp))
                } else {
                  // Decl hasn't changed
                  None
                }
              case None =>
                // The def has been deleted
                None
            }
          case None =>
            // It's a newly added def
            None
        })
    newlyAddedDefs ++ changedDefs
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
