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

import ca.uwaterloo.flix.api.lsp.acceptors.{FileAcceptor}
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Root}
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.Visitor

object InlayHintProvider {
  def getInlayHints(uri: String, range: Range)(implicit root: Root): List[InlayHint] = {
    var inlayHints: List[InlayHint] = Nil
    object opSymUseConsumer extends Consumer {
      override def consumeOpSymUse(opSymUse: SymUse.OpSymUse): Unit = {
        val line = opSymUse.loc.beginLine
        var hint = InlayHint(
          position = Position(line, 100),
          label = opSymUse.sym.eff.name,
          kind = Some(InlayHintKind.Type),
          textEdits = List.empty,
          tooltip = s"Effect: ${opSymUse.sym.eff.name}",
          paddingLeft = true,
          paddingRight = true
        )
      inlayHints = hint :: inlayHints
      }
    }

    Visitor.visitRoot(root, opSymUseConsumer, FileAcceptor(uri))

    inlayHints
  }
}
