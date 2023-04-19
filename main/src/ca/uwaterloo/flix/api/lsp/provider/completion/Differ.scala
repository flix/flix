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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}

object Differ {

  /**
    * Computes the semantic difference between the `oldAst` and `newAst`
    */
  def difference(old: Option[TypedAst.Root], newAst: TypedAst.Root): DeltaContext = old match {
    case None =>
      // Case 1: No old AST. No difference.
      DeltaContext(Map.empty)
    case Some(oldAst) =>
      // Case 2: We have an oldAst and a newAst. Compute their difference.

      val newDefs = findModifiedDefs(oldAst, newAst)

      DeltaContext(newDefs)
  }

  /**
    * Find all defs that has been modified.
    *
    * This is done by checking if a def in an oldAst is different from that same def in the newAst.
    * If the def is modified, we add it to the delta.
    *
    * @param oldAst the old AST.
    * @param newAst the new AST.
    * @return       a new DeltaContext containing a delta with a map from DefnSym to timestamp.
    */
  private def findModifiedDefs(oldAst: TypedAst.Root, newAst: TypedAst.Root): Map[Symbol.DefnSym, Long] = {
    newAst.defs.flatMap {
      case (sym, newDef) =>
        // Check if the sym also exists in the oldAst
        oldAst.defs.get(sym) match {
          case None =>
            // It's a newly added def, add it to the deltas
            Some((sym, getCurrentTimestamp))
          case Some(oldDef) =>
            // We have an old def, check if the implementation has changed
            if (isDefModified(oldDef, newDef)) {
              // The def has been modified, add it to the deltas
              Some((sym, getCurrentTimestamp))
            } else {
              // The def hasn't changed, don't add it to the deltas.
              None
            }
        }
    }
  }

  /**
    * Compares two defs in terms of change.
    *
    * @param oldDef the old def.
    * @param newDef the new def.
    * @return       true if the def has been modified, false otherwise.
    */
  private def isDefModified(oldDef: TypedAst.Def, newDef: TypedAst.Def): Boolean = {
    isSourceLikelyChanged(oldDef.impl.exp.loc, newDef.impl.exp.loc)
  }

  /**
    * Checks if two locations have equal span.
    *
    * This function is different from SourceLocation.equals().
    * This only checks if the location of a specific node in the AST has the same span as the other.
    *
    * @param loc1 the first location.
    * @param loc2 the second location.
    * @return     true if the location has changed, false otherwise.
    */
  private def isSourceLikelyChanged(loc1: SourceLocation, loc2: SourceLocation): Boolean =
  // Check if both SourceLocations only spans a single line
    (loc1.isSingleLine, loc2.isSingleLine) match {
      case (true, false) | (false, true) =>
        // We have a change
        true
      case (true, true) =>
        // Not necessarily changed
        // Check if columnSpan has changed
        columnSpan(loc1) != columnSpan(loc2)
      case (false, false) =>
        // Not necessarily changed
        // Check if columnSpan or lineSpan has changed
        columnSpan(loc1) != columnSpan(loc2) ||
          lineSpan(loc1) != lineSpan(loc2)
    }


  /**
    * Calculates the number of lines the SourceLocation spans.
    *
    * @param loc the SourceLocation.
    * @return    the number of lines the SourceLocation spans.
    */
  private def lineSpan(loc: SourceLocation): Int =
    loc.endLine - loc.beginLine

  /**
    * Calculates the number of columns the SourceLocation spans.
    *
    * @param loc the SourceLocation.
    * @return    the number of columns the SourceLocation spans.
    */
  private def columnSpan(loc: SourceLocation): Int =
    loc.endCol - loc.beginCol


  /**
    * Returns the current timestamp.
    */
  private def getCurrentTimestamp: Long = System.nanoTime()
}
