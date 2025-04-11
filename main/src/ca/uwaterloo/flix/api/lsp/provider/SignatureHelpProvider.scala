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
import ca.uwaterloo.flix.api.lsp.{LspUtil, Position, SignatureHelp, SignatureInformation}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root

object SignatureHelpProvider {
  /**
    * Provides signature help for the given position.
    */
  def provideSignatureHelp(uri: String, pos: Position)(implicit root: Root, flix: Flix): Option[SignatureHelp] = {
    LspUtil.getStack(uri, pos).collectFirst {
      // Find the nearest function application, that is, the lowest ApplyDef in the AST that contains the given position
      case TypedAst.Expr.ApplyDef(defnSymUse, exps, _, _, _, _) =>
        // Count the number of arguments applied
        // The number of arguments applied is the number of non-synthetic expressions
        val argsNumApplied = exps.indexWhere(exp => pos.containedBy(exp.loc))
        val defn = root.defs(defnSymUse.sym)
        val signatureInfo = SignatureInformation.from(defn, argsNumApplied)
        SignatureHelp(List(signatureInfo), 0, 0)
    }
  }
}
