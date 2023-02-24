/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Entity, Index}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.VarCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object VarCompleter extends Completer {
  /**
    * Returns a List of Completion for var.
    */
  override def getCompletions(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[VarCompletion] = {
    if (root.isEmpty) {
      return Nil
    }

    ///
    /// Find all local variables in the current uri with a given range.
    ///
    val iter = index.queryWithRange(context.uri, queryLine = context.range.start.line, beforeLine = 20, afterLine = 10).collect {
      case Entity.LocalVar(sym, tpe) => Completion.VarCompletion(sym, tpe, context, flix)
      case Entity.FormalParam(fparam) => Completion.VarCompletion(fparam.sym, fparam.tpe, context, flix)
    }

    iter.toList
  }
}
