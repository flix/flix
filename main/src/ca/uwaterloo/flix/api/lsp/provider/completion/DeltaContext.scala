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
  * Represents changes between ASTs.
  */
object DeltaContext {
  /**
    * Merges two DeltaContext.
    *
    * Removes duplicates that has an older timestamp, and deltas older than 5 minutes.
    *
    * Limits deltas to 1000 elements.
    *
    * Sorts the list with the most recent delta as first element.
    *
    * @param d1  the first DeltaContext.
    * @param d2  the second DeltaContext.
    * @return    a DeltaContext with the of changes as List[Delta],
    *            where the most recent is the first element of the list.
    */
  def mergeDeltas(d1: DeltaContext, d2: DeltaContext): DeltaContext = {
    val newDeltas = {
      if (d2.deltas.isEmpty) {
        d1.deltas
      } else {
        val (limitDeltas, _) = filterOutdated(d1.deltas ++ d2.deltas)
            .splitAt(1000) // Limits the list to the 1000 most recent deltas
        limitDeltas
      }
    }
    DeltaContext(newDeltas)
  }

  /**
    * Removes all duplicates, so only the most recent appear in the list.
    *
    * Sorts the list with the most recent delta as first element.
    *
    * Also removes all deltas older than 5 minutes.
    *
    * @param deltas the list of deltas.
    * @return       a new list of deltas without duplicates, older deltas than 5 minutes and in sorted order.
    */
  private def filterOutdated(deltas: List[Delta]): List[Delta] = {
    // Remove all duplicates.
    // Reverses the list so the most recent deltas appears first.
    // This ensures that all duplicates with older timestamp will be removed.
    val deltaWithoutDuplicates = deltas.reverse.distinctBy {
      case Delta.ModifiedDef(sym, _) => sym
      case Delta.AddEnum(sym, _) => sym
      case Delta.AddField(field, _) => field
    }
    val fiveMinTimestamp = Differ.getCurrentTimestamp - 300000000000L
    deltaWithoutDuplicates
      .sortWith(_.timestamp > _.timestamp) // Sorts the list, to make sure the newest timestamp is the first elem.
      .takeWhile(_.timestamp > fiveMinTimestamp) // Remove deltas other than 5 minutes
  }
}

/**
  * Represents a list of changes.
  */
case class DeltaContext(deltas: List[Delta]) {

  def isNewEnum(sym: Symbol.EnumSym): Boolean = deltas.exists {
    case Delta.AddEnum(sym2, _) => sym == sym2 // TODO: And time?
    case _ => false
  }
}
