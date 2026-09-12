/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.OpHandlerCompletion
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}

object HandlerCompleter {

  def getCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[OpHandlerCompletion] = {
    val effSym = Symbol.mkEffSym(qn.namespace.toString)
    root.effects.get(effSym).toList.flatMap(eff =>
      eff.ops.collect {
        case op if CompletionUtils.isAvailable(eff) && CompletionUtils.matchesName(op.sym, qn, qualified = false) =>
          OpHandlerCompletion(op, range, Priority.High(0))
      }
    )
  }
}
