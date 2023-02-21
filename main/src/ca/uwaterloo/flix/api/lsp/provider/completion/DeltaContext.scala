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

  def isNewEnum(sym: Symbol.EnumSym): Boolean = deltas.exists {
    case Delta.AddEnum(sym2, _) => sym == sym2 // TODO: And time?
    case _ => false
  }

}
