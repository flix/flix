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

/** Represents an `InsertTextFormat` in LSP. */
sealed trait InsertTextFormat {
  def toInt: Int = this match {
    case InsertTextFormat.PlainText => 1
    case InsertTextFormat.Snippet => 2
  }
}

object InsertTextFormat {

  /** The primary text to be inserted is treated as a plain string. */
  case object PlainText extends InsertTextFormat

  /**
    *
    * The primary text to be inserted is treated as a snippet.
    *
    * A snippet can define tab stops and placeholders with `$1`, `$2`
    * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
    * the end of the snippet. Placeholders with equal identifiers are
    * linked, that is typing in one will update others too.
    */
  case object Snippet extends InsertTextFormat

}
