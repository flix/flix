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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.DefCompletion
import ca.uwaterloo.flix.api.lsp.{LspUtil, Position, Range}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Def
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError.UndefinedName

object DefCompleter {
  /**
    * Returns a List of Completion for definitions.
    * Whether the returned completions are qualified is based on whether the UndefinedName is qualified.
    * When providing completions for unqualified defs that is not in scope, we will also automatically use the def.
    */
  def getCompletions(uri: String, pos: Position, qn: Name.QName, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: Root, flix: Flix): Iterable[Completion] = {
    val ectx = getExprContext(uri, pos)

    if (qn.namespace.nonEmpty) {
      root.defs.values.collect {
        case decl if CompletionUtils.isAvailable(decl.spec) && CompletionUtils.matchesName(decl.sym, qn, qualified = true) =>
          DefCompletion(decl, range, ap, qualified = true, inScope = true, ectx)
      }
    } else {
      root.defs.values.collect {
        case decl if CompletionUtils.isAvailable(decl.spec) && CompletionUtils.matchesName(decl.sym, qn, qualified = false) =>
          DefCompletion(decl, range, ap, qualified = false, inScope = inScope(decl, scp), ectx)
      }
    }
  }

  /**
    * Checks if the definition is in scope.
    * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
    */
  private def inScope(decl: TypedAst.Def, scope: LocalScope): Boolean = {
    val thisName = decl.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Def(thatName, _, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = decl.sym.namespace.isEmpty
    isRoot || isResolved
  }

  /**
    * Returns the expression context at the given `uri` and position `pos`.
    */
  private def getExprContext(uri: String, pos: Position)(implicit root: Root, flix: Flix): ExprContext = {
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
