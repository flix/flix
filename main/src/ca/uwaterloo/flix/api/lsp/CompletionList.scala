/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `CompletionList` in LSP.
  *
  * @param isIncomplete This list is not complete. Further typing should result in recomputing this list.
  *                     Recomputed lists have all their items replaced (not appended) in the incomplete completion sessions.
  * @param items        The completion items.
  */
case class CompletionList(isIncomplete: Boolean, items: Iterable[CompletionItem]) {
  def toJSON: JValue = ("isIncomplete" -> isIncomplete) ~ ("items" -> items.map(_.toJSON))
}
