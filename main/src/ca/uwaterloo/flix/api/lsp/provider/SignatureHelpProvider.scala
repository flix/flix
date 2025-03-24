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

package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.acceptors.InsideAcceptor
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.{Position, SignatureHelp, SignatureInformation, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root

object SignatureHelpProvider {
  /**
    * Provides signature help for the given position.
    */
  def provideSignatureHelp(uri: String, pos: Position)(implicit root: Root, flix: Flix): Option[SignatureHelp] = {
    val stack = StackConsumer()

    if (pos.character >= 2) {
      val leftPos = Position(pos.line, pos.character - 1)
      Visitor.visitRoot(root, stack, InsideAcceptor(uri, leftPos))
    }

    stack.getStack.collectFirst {
      // Find the nearest function application
      case TypedAst.Expr.ApplyDef(defnSymUse, exps, _, _, _, _) =>
        // Count the number of arguments applied
        // The number of arguments applied is the number of non-synthetic expressions
        val argsNumApplied = exps.indexWhere { exp =>
          pos.line == exp.loc.sp2.line && pos.character <= exp.loc.sp2.col && pos.character >= exp.loc.sp1.col
        }
        val defn = root.defs(defnSymUse.sym)
        val signatureInfo = SignatureInformation.from(defn, argsNumApplied)
        SignatureHelp(List(signatureInfo), 0, 0)
    }
  }

}
