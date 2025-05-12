/*
 * Copyright 2025 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpHandlerCompletion
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object HandlerCompleter {

  def getCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[OpHandlerCompletion] = {
    val effSym = Symbol.mkEffectSym(qn.namespace.toString)
    root.effects.get(effSym).toList.flatMap (eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
          OpHandlerCompletion(op, range)
      }
    )
  }
}
