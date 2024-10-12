/*
 * Copyright 2020 Magnus Madsen
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

import org.json4s.JsonDSL.*
import org.json4s.*
import ca.uwaterloo.flix.language.ast.Range

/**
  * Represents a `TextEdit` in LSP.
  *
  * @param range   The range of the text document to be manipulated.
  *                To insert text into a document create a range where start === end.
  * @param newText The string to be inserted. For delete operations use an empty string.
  */
case class TextEdit(range: Range, newText: String) {
  def toJSON: JValue = ("range" -> range.toJSON) ~ ("newText" -> newText)
}
