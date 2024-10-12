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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.Position
import org.json4s.JsonDSL.*
import org.json4s.JValue

/**
  * Represent `InlayHint` in LSP.
  * @since 3.17.0
  *
  * @param position     The position of this hint
  * @param label        The label of this hint. A human readable string or an array of
  *                     InlayHintLabelPart label parts.
  *                     *Note* that neither the string nor the label part can be empty.
  * @param kind         The kind of this hint. Can be omitted in which case the client
  *                     should fall back to a reasonable default.
  * @param textEdits    Optional text edits that are performed when accepting this inlay hint.
  *                     *Note* that edits are expected to change the document so that the inlay
  *                     hint (or its nearest variant) is now part of the document and the inlay
  *                     hint itself is now obsolete.
  *                     Depending on the client capability `inlayHint.resolveSupport` clients
  *                     might resolve this property late using the resolve request.
  * @param tooltip      The tooltip text when you hover over this item.
  *                     Depending on the client capability `inlayHint.resolveSupport` clients
  *                     might resolve this property late using the resolve request.
  * @param paddingLeft  Render padding before the hint.
  *                     Note: Padding should use the editor's background color, not the
  *                     background color of the hint itself. That means padding can be used
  *                     to visually align/separate an inlay hint.
  * @param paddingRight Render padding after the hint.
  *                     Note: Padding should use the editor's background color, not the
  *                     background color of the hint itself. That means padding can be used
  *                     to visually align/separate an inlay hint.
  */
case class InlayHint(position: Position, label: String, kind: Option[InlayHintKind], textEdits: List[TextEdit], tooltip: String, paddingLeft: Boolean = true, paddingRight: Boolean = true) {
  def toJSON: JValue =
    ("position" -> position.toJSON) ~
      ("label" -> label) ~
      ("kind" -> kind.map(_.toJSON).orNull) ~
      ("textEdits" -> textEdits.map(_.toJSON)) ~
      ("tooltip" -> tooltip) ~
      ("paddingLeft" -> paddingLeft) ~
      ("paddingRight" -> paddingRight)
}
