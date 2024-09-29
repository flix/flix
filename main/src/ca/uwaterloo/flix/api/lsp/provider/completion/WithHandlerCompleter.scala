/*
 * Copyright 2024 Holger Dal Mogensen
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
import ca.uwaterloo.flix.api.lsp.{Index, TextEdit}
import ca.uwaterloo.flix.language.ast.TypedAst

object WithHandlerCompleter {
  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
    if (context.previousWord != "with") {
      return Nil
    }

    // TODO: infer the effect of the expression
    root.effects.map { case (sym, eff) =>
      val effString = sym.name
      val opStrings = eff.ops.map(fmtOp)
      val bodyString = s"$effString {\n${opStrings.mkString("\n")}\n}"
      Completion.WithHandlerCompletion(effString, TextEdit(context.range, bodyString))
    }
  }

  private def fmtOp(op: TypedAst.Op): String = {
    val fparamsString = (op.spec.fparams.map(p => p.sym.text) :+ "k").mkString(", ")
    s"    def ${op.sym.name}($fparamsString) = ???"
  }
}
