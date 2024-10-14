/*
 * Copyright 2021 Magnus Madsen
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

import org.json4s.*
import org.json4s.JsonDSL.*

/**
  * Companion object of [[CompletionItem]].
  */
object CompletionItem {

}

/**
  * Represents a `CompletionItem` in LSP.
  *
  * @param label            The label of this completion item. By default also the text that is inserted when selecting this completion.
  * @param sortText         A string that should be used when sorting completion items
  * @param filterText       A string that should be used when filtering a set of completion items. When `falsy` the label is used
  *                         as the filter text for this item.
  * @param textEdit         An edit which is applied to a document when selecting this completion. *Note:* The range of the edit must be
  *                         a single line range and it must contain the position at which completion has been requested.
  * @param detail           A human-readable string with additional information about this item, like type or symbol information.
  * @param documentation    A human-readable string that represents a doc-comment.
  * @param kind             The kind of this completion item. Based of the kind an icon is chosen by the editor. The standardized set of available values is defined in `CompletionItemKind`.
  * @param insertTextFormat The format of the insert text. The format applies to both the `insertText` property and the `newText` property
  *                         of a provided `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
  * @param commitCharacters An optional set of characters that when pressed while this completion is active will accept it first and
  *                         then type that character. *Note* that all commit characters should have `length=1` and that superfluous characters
  *                         will be ignored.
  */
case class CompletionItem(
  label: String,
  sortText: String,
  filterText: Option[String] = None,
  textEdit: TextEdit,
  detail: Option[String] = None,
  documentation: Option[String] = None,
  kind: CompletionItemKind,
  insertTextFormat: InsertTextFormat = InsertTextFormat.PlainText,
  commitCharacters: List[String] = Nil) {

  def toJSON: JValue =
    ("label" -> label) ~
      ("sortText" -> sortText) ~
      ("filterText" -> filterText) ~
      ("textEdit" -> textEdit.toJSON) ~
      ("detail" -> detail) ~
      ("documentation" -> documentation) ~
      ("kind" -> kind.toInt) ~
      ("insertTextFormat" -> insertTextFormat.toInt) ~
      ("commitCharacters" -> commitCharacters)
}
