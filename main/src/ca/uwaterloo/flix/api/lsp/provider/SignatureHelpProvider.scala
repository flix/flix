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
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}

object SignatureHelpProvider {
  /**
    * Provides signature help for the given position.
    * We find the nearest application, which is the lowest ApplyDef/ApplySig in the AST that contains the position of the cursor.
    */
  def provideSignatureHelp(uri: String, pos: Position)(implicit root: Root, flix: Flix): Option[SignatureHelp] = {
    LspUtil.getStack(uri, pos).collectFirst {
      case TypedAst.Expr.ApplyDef(defnSymUse, exps, _, _, _, _) =>
        buildSignatureHelp(defnSymUse.sym, root.defs(defnSymUse.sym).spec, exps, pos)
      case TypedAst.Expr.ApplySig(sigSymUse, exps, _, _, _, _) =>
        buildSignatureHelp(sigSymUse.sym, root.sigs(sigSymUse.sym).spec, exps, pos)
    }
  }

  /**
    * Builds the signature help for the given symbol and its specification.
    *
    * @param symbol the symbol of the function/signature.
    * @param spec   the specification of the function/signature.
    * @param exps   the expressions passed as arguments to the function/signature.
    * @param pos    the position of the cursor.
    * @return a SignatureHelp object containing the signature information.
    */
  private def buildSignatureHelp(symbol: Symbol, spec: TypedAst.Spec, exps: List[TypedAst.Expr], pos: Position)(implicit flix: Flix): SignatureHelp = {
    // Count the index of the active parameter, which is the first expression that contains the position of the cursor.
    val idxActiveParameter = exps.indexWhere(exp => pos.containedBy(exp.loc))
    val signatureInfo = SignatureInformation.from(symbol, spec, idxActiveParameter)
    SignatureHelp(List(signatureInfo), 0, 0)
  }
}
