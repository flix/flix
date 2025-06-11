/*
 * Copyright 2022 Nicola Dardanis
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
import ca.uwaterloo.flix.api.lsp.acceptors.{FileAcceptor, AllAcceptor}
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Op, Root}
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.Visitor


object InlayHintProvider {
  private val EnableEffectHints: Boolean = true

  def getInlayHints(uri: String, range: Range)(implicit root: Root): List[InlayHint] = {
    var inlayHints: List[InlayHint] = Nil
    if(EnableEffectHints) {
      var opSymUses: Set[(SymUse.OpSymUse, SourceLocation)] = getOpSymUses(uri)
      opSymUses.foreach { case (opSymUse, loc) => mkHintFromOpSymUse(opSymUse, loc) :: inlayHints }
    }
    inlayHints
  }

  def getOpSymUses(uri: String)(implicit root: Root): Set[(SymUse.OpSymUse, SourceLocation)] = {
    var opSymUses: Set[(SymUse.OpSymUse, SourceLocation)] = Set.empty
    object opSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.Do(opSymUse, _, _, _, loc) =>
            opSymUses += ((opSymUse, loc))
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, opSymUseConsumer, FileAcceptor(uri))
    opSymUses
  }

  def mkHintFromOpSymUse(opSymUse: SymUse.OpSymUse, loc: SourceLocation): InlayHint = {
    InlayHint(
      position = Position(loc.endLine, loc.endCol + 1),
      label = opSymUse.sym.eff.name,
      kind = Some(InlayHintKind.Type),
      textEdits = List.empty,
      tooltip = s"{ ${opSymUse.sym.eff.name} }",
      paddingLeft = true,
      paddingRight = true
    )
  }
}
