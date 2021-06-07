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

import org.json4s.JsonDSL._
import org.json4s._

/**
  * Companion object of [[CompletionItem]].
  */
object CompletionItem {

}

/**
  * Represents a `CompletionItem` in LSP.
  *
  * @param label            The label of this completion item. By default also the text that is inserted when selecting this completion.
  * @param detail           A human-readable string with additional information about this item, like type or symbol information.
  * @param insertTextFormat The format of the insert text. The format applies to both the `insertText` property and the `newText` property
  *                         of a provided `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
  */
case class CompletionItem(label: String, insertText: String, detail: Option[String], insertTextFormat: InsertTextFormat) {
  def toJSON: JValue =
    ("label" -> label) ~
      ("insertText" -> insertText) ~
      ("detail" -> detail) ~
      ("insertTextFormat" -> insertTextFormat.toInt)
}
