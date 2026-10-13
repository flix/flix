/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.KindCompletion

object KindCompleter {
  def getCompletions(name: String, range: Range): List[Completion] = {
    val kinds = List("Type", "Eff", "Bool")
    kinds.collect {
      case kind if kind.startsWith(name) => KindCompletion(kind, range, Priority.Highest(0))
    }
  }
}
