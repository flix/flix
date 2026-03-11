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

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.acceptors.InsideAcceptor
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.provider.completion.{CompletionUtils, ExprContext}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

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
  def getStack(uri: String, pos: Position)(implicit root: Root, flix: Flix): List[AnyRef] = {
    val stack = StackConsumer()

    if (pos.character >= 2) {
      val leftPos = Position(pos.line, pos.character - 1)
      Visitor.visitRoot(root, stack, InsideAcceptor(uri, leftPos))
    }

    stack.getStack
  }

  /**
    * Generates a user-friendly label for the given function specification.
    *
    * The label includes parameter names and types, return type, and effect (if applicable),
    * formatted in a readable way.
    */
  def getLabelForSpec(spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, eff0, _, _) =>
      val args = if (fparams.length == 1 && fparams.head.tpe == Type.Unit)
        Nil
      else
        fparams.map {
          fparam => s"${fparam.bnd.sym.text}: ${FormatType.formatType(fparam.tpe)}"
        }

      val retTpe = FormatType.formatType(retTpe0)

      val eff = eff0 match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case p => raw" \ " + FormatType.formatType(p)
      }

      s"(${args.mkString(", ")}): $retTpe$eff"
  }

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
    case ExprContext.InsidePipeline => CompletionUtils.getApplySnippet(label, spec.fparams.dropRight(1))
    case ExprContext.InsideRunWith => CompletionUtils.getApplySnippet(label, spec.fparams.dropRight(1))
    case ExprContext.Unknown => CompletionUtils.getApplySnippet(label, spec.fparams)
  }
}
