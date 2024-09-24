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

/** Represents a `CompletionItemKind` in LSP. */
sealed trait CompletionItemKind {
  def toInt: Int = this match {
    case CompletionItemKind.Text => 1
    case CompletionItemKind.Method => 2
    case CompletionItemKind.Function => 3
    case CompletionItemKind.Constructor => 4
    case CompletionItemKind.Field => 5
    case CompletionItemKind.Variable => 6
    case CompletionItemKind.Class => 7
    case CompletionItemKind.Interface => 8
    case CompletionItemKind.Module => 9
    case CompletionItemKind.Property => 10
    case CompletionItemKind.Unit => 11
    case CompletionItemKind.Value => 12
    case CompletionItemKind.Enum => 13
    case CompletionItemKind.Keyword => 14
    case CompletionItemKind.Snippet => 15
    case CompletionItemKind.Color => 16
    case CompletionItemKind.File => 17
    case CompletionItemKind.Reference => 18
    case CompletionItemKind.Folder => 19
    case CompletionItemKind.EnumMember => 20
    case CompletionItemKind.Constant => 21
    case CompletionItemKind.Struct => 22
    case CompletionItemKind.Event => 23
    case CompletionItemKind.Operator => 24
    case CompletionItemKind.TypeParameter => 25
  }
}

object CompletionItemKind {
  case object Text extends CompletionItemKind

  case object Method extends CompletionItemKind

  case object Function extends CompletionItemKind

  case object Constructor extends CompletionItemKind

  case object Field extends CompletionItemKind

  case object Variable extends CompletionItemKind

  case object Class extends CompletionItemKind

  case object Interface extends CompletionItemKind

  case object Module extends CompletionItemKind

  case object Property extends CompletionItemKind

  case object Unit extends CompletionItemKind

  case object Value extends CompletionItemKind

  case object Enum extends CompletionItemKind

  case object Keyword extends CompletionItemKind

  case object Snippet extends CompletionItemKind

  case object Color extends CompletionItemKind

  case object File extends CompletionItemKind

  case object Reference extends CompletionItemKind

  case object Folder extends CompletionItemKind

  case object EnumMember extends CompletionItemKind

  case object Constant extends CompletionItemKind

  case object Struct extends CompletionItemKind

  case object Event extends CompletionItemKind

  case object Operator extends CompletionItemKind

  case object TypeParameter extends CompletionItemKind

}
