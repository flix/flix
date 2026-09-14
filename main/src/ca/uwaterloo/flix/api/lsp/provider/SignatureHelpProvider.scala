/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
      case TypedAst.Expr.ApplyDef(defnSymUse, exps, _, _, _, _, _, _) =>
        // The lookup is guaranteed to succeed because otherwise the expression would be replaced by Expr.Error.
        mkSignatureHelp(defnSymUse.sym, root.defs(defnSymUse.sym).spec, exps, pos)
      case TypedAst.Expr.ApplyOp(opSymUse, exps, _, _, _, _) =>
        // The lookup is guaranteed to succeed because otherwise the expression would be replaced by Expr.Error.
        val ops = root.effects(opSymUse.sym.eff).ops
        val op = ops.find(thatOp => thatOp.sym == opSymUse.sym).get
        mkSignatureHelp(opSymUse.sym, op.spec, exps, pos)
      case TypedAst.Expr.ApplySig(sigSymUse, exps, _, _, _, _, _, _, _) =>
        // The lookup is guaranteed to succeed because otherwise the expression would be replaced by Expr.Error.
        mkSignatureHelp(sigSymUse.sym, root.sigs(sigSymUse.sym).spec, exps, pos)
    }
  }

  /**
    * Builds the signature help for the given symbol and its specification.
    *
    * @param sym  the symbol of the function/signature.
    * @param spec the specification of the function/signature.
    * @param exps the expressions passed as arguments to the function/signature.
    * @param pos  the position of the cursor.
    * @return a SignatureHelp object containing the signature information.
    */
  private def mkSignatureHelp(sym: Symbol, spec: TypedAst.Spec, exps: List[TypedAst.Expr], pos: Position)(implicit flix: Flix): SignatureHelp = {
    // Count the index of the active parameter, which is the first expression that contains the position of the cursor.
    val activeParameter = exps.indexWhere(exp => pos.containedBy(exp.loc))
    val signatureInfo = SignatureInformation.from(sym, spec, activeParameter)
    SignatureHelp(List(signatureInfo), 0, 0)
  }
}
