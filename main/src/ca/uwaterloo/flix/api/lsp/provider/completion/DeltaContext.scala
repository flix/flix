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
case class DeltaContext(deltas: List[Delta]) {

  /**
    * Merges this.List[Delta] with other.List[Delta].
    * Removes duplicates that has an older timestamp.
    *
    * @param other the other DeltaContext.
    * @return      a DeltaContext with the of changes as List[Delta],
    *              where the most recent is the first elem of the list.
    */
  def mergeDeltas(other: DeltaContext): DeltaContext = {
    val newDeltas = {
      if (other.deltas.isEmpty) {
        this.deltas
      } else {
        (this.deltas ++ other.deltas).reverse.distinctBy {
          case Delta.ModifiedDef(sym, _) => sym
          case Delta.AddEnum(sym, _) => sym
          case Delta.AddField(field, _) => field
        }.sortWith(_.timestamp > _.timestamp) // Sorts the list, to make sure the newest timestamp is the first elem.
      }
    }
    DeltaContext(newDeltas)
  }

  def isNewEnum(sym: Symbol.EnumSym): Boolean = deltas.exists {
    case Delta.AddEnum(sym2, _) => sym == sym2 // TODO: And time?
    case _ => false
  }
}
