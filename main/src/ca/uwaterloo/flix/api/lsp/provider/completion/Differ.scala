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

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

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
      val newDefs = findModifiedDefs(oldAst, newAst)
      val newEnums = (newAst.enums.keySet -- oldAst.enums.keySet).toList.map(sym => Delta.AddEnum(sym, getCurrentTimestamp))

      DeltaContext(newDefs ++ newEnums)
  }

  /**
    * Find all defs that has been modified.
    *
    * This is done by checking if a def in an oldAst is different from that same def in the newAst.
    * If the def is modified, we add it to the list of deltas.
    *
    * @param oldAst the old AST.
    * @param newAst the new AST.
    * @return       a List[Delta] consisting of newly modified defs.
    */
  private def findModifiedDefs(oldAst: TypedAst.Root, newAst: TypedAst.Root): List[Delta] = {
    newAst.defs.flatMap {
      case (sym, newDef) =>
        // Check if the sym also exists in the oldAst
        oldAst.defs.get(sym) match {
          case None =>
            // It's a newly added def, add it to the deltas
            Some(Delta.ModifiedDef(sym, getCurrentTimestamp))
          case Some(oldDef) =>
            // We have an old def, check if the implementation has changed
            if (isDefModified(oldDef, newDef)) {
              // The def has been modified, add it to the deltas
              Some(Delta.ModifiedDef(sym, getCurrentTimestamp))
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
    * @return       true if the def has been modified, false otherwise.
    */
  private def isDefModified(oldDef: TypedAst.Def, newDef: TypedAst.Def): Boolean = {
    !equalLocationSpan(oldDef.impl.exp.loc, newDef.impl.exp.loc)
  }

  /**
    * Checks if two locations have equal span.
    *
    * This function is different from SourceLocation.equals().
    * This only checks if the location of a specific node in the AST has the same span as the other.
    *
    * @param loc1 the first location.
    * @param loc2 the second location.
    * @return     true if the location is the same, false otherwise.
    */
  private def equalLocationSpan(loc1: SourceLocation, loc2: SourceLocation): Boolean = {
    loc1.beginCol == loc2.beginCol &&
      loc1.endCol == loc2.endCol &&
      loc1.beginLine - loc1.endLine == loc2.beginLine - loc2.endLine
  }

  /**
    * Returns the current timestamp.
    */
  def getCurrentTimestamp: Long = System.nanoTime()

}
