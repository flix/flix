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

import ca.uwaterloo.flix.language.ast.Symbol

trait ModuleSymFragment

object ModuleSymFragment {
  /**
    * Represents a partial module sym, i.e. `Aaa.Bbb.Ccc`.
    *
    * The partially comes from the fact that user could type more letters (Cs).
    */
  case class Partial(sym: Symbol.ModuleSym, suffix: String) extends ModuleSymFragment

  /**
    * Represents a complete module sym, i.e. `Aaa.Bbb.Ccc.`.
    *
    * The completeness comes from the fact that the user has typed ".".
    */
  case class Complete(sym: Symbol.ModuleSym) extends ModuleSymFragment

  /**
    * Parses a partial or complete module symbol.
    *
    * `Aaa.Bbb.Ccc.` => Complete(Aaa.Bbb.Ccc)
    * `Aaa.Bbb.`     => Complete(Aaa.Bbb)
    * `Aaa`          => Partial(Nil, Aaa)
    * `Aaa.Bbb.Ccc`  => Partial(Aaa.Bbb, Ccc)
    * <empty>        => Partial(Nil, "")
    */

  def parseModuleSym(word: String): ModuleSymFragment =
    if (word.endsWith(".")) {
      // Case 1: Complete.
      ModuleSymFragment.Complete(Symbol.mkModuleSym(word.split('.').toList))
    } else {
      // Case 2: Partial: Split into module part and suffix part.
      // Fortunately, splitting on '.' will just drop '.' if it is last.
      // Thus we simply need the init and the last element, if the list is non-empty.
      val parts = word.split('.').toList
      if (parts.isEmpty) {
        ModuleSymFragment.Partial(Symbol.mkModuleSym(Nil), "")
      } else {
        ModuleSymFragment.Partial(Symbol.mkModuleSym(parts.init), parts.last) // Safe because parts is non-empty.
      }
    }
}
