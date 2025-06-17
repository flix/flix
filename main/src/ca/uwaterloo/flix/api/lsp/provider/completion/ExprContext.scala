/*
 * Copyright 2025 Magnus Madsen
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
import ca.uwaterloo.flix.api.lsp.{LspUtil, Position}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.errors.ResolutionError.UndefinedName

sealed trait ExprContext

object ExprContext {

  /**
    * Represents an expression in an application context.
    *
    * For example, in `e1(e2, e3)` we have that `e1` is `UnderApply` but `e2` and `e3` are not.
    */
  case object InsideApply extends ExprContext

  /**
    * Represents an expression in a match context.
    *
    * For example, in `1 |> println` we have that `println` is `UnderPipeline`.
    *
    * Currently, the only pipeline considered is `|>`, so the number of arguments from the pipeline is always 1.
    */
  case object InsidePipeline extends ExprContext

  /**
    * Represents an expression in a run with context.
    *
    * For example, in `run { e1 } with e2` we have that `e2` is `InsideRunWith` while `e1` is not.
    */
  case object InsideRunWith extends ExprContext

  /**
    * Represents an expression in an unknown context.
    */
  case object Unknown extends ExprContext

  /**
    * Returns the expression context at the given `uri` and position `pos`.
    */
  def getExprContext(uri: String, pos: Position)(implicit root: Root, flix: Flix): ExprContext = {
    val stack = LspUtil.getStack(uri, pos)
    // The stack contains the path of expressions from the leaf to the root.
    stack match {
      case Expr.Error(UndefinedName(_, _, _, _), _, _) :: Expr.ApplyClo(_, _, _, _, _) :: _ =>
        // The leaf is an error followed by an ApplyClo expression.
        ExprContext.InsideApply
      case Expr.Error(UndefinedName(_, _, _, _), _, _) :: Expr.ApplyDef(DefSymUse(sym, _), _, _, _, _, _) :: _ if sym.text == "|>" =>
        // The leaf is an error followed by an ApplyDef expression with the symbol "|>".
        ExprContext.InsidePipeline
      case Expr.Error(UndefinedName(_, _, _, _), _, _) :: Expr.RunWith(_, _, _, _, _) :: _ =>
        // The leaf is an error followed by a RunWith expression.
        ExprContext.InsideRunWith
      case _ => ExprContext.Unknown
    }
  }

}
