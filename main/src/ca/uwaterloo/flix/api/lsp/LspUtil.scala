/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.acceptors.InsideAcceptor
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.provider.completion.{CompletionUtils, ExprContext}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SourceName
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.fmt.{Pretty, RichText}

object LspUtil {
  /**
    * Returns the stack of AST nodes that contains the given position.
    *
    * The stack is constructed from a visitor with an InsideAcceptor.
    *
    * Given that:
    * - We have to visit an AST node's parent before visiting the node itself
    * - If the given position is contained by an AST node, it will also be contained by the node's parent
    *
    * So the stack actually contains a path from the leaf node that contains the given position to the root node, with the leaf node at the top of the stack.
    */
  def getStack(name: SourceName, pos: Position)(implicit root: Root): List[AnyRef] = {
    val stack = StackConsumer()

    if (pos.character >= 2) {
      val leftPos = Position(pos.line, pos.character - 1)
      Visitor.visitRoot(root, stack, InsideAcceptor(name, leftPos))
    }

    stack.getStack
  }

  /**
    * Generates a user-friendly label for the given function specification.
    *
    * The label includes parameter names and types, return type, and effect (if applicable),
    * formatted in a readable way.
    */
  def getLabelForSpec(spec: TypedAst.Spec): String =
    (Pretty.fparams(spec.fparams) ++ RichText.text(": ") ++ Pretty.typeAndEff(spec.retTpe, spec.eff)).plain

  /**
    * Generates a user-friendly snippet for the given spec.
    * The spec could come from a function, a signature or an op.
    *
    * If the spec is inside an apply, like fo(...), we will only complete the label.
    * If the spec is inside a pipeline, like 1 |> fo(...), we will complete the snippet with the rightmost parameter dropped.
    * If the spec is inside a run with, like run { e1 } with fo(...), we will complete the snippet with the rightmost parameter dropped.
    * Otherwise, we will complete the snippet with all parameters.
    */
  def mkSpecSnippet(label: String, spec: TypedAst.Spec, ectx: ExprContext): String = ectx match {
    case ExprContext.InsideApply => CompletionUtils.getApplySnippet(label, Nil)
    case ExprContext.InsidePipeline => CompletionUtils.getApplySnippet(label, spec.fparams.init)
    case ExprContext.InsideRunWith => CompletionUtils.getApplySnippet(label, spec.fparams.init)
    case ExprContext.Unknown => CompletionUtils.getApplySnippet(label, spec.fparams.toList)
  }
}
