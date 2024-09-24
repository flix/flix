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

/** Represents a `SymbolKind` in LSP. */
sealed trait SymbolKind {
  def toInt: Int = this match {
    case SymbolKind.File => 1
    case SymbolKind.Module => 2
    case SymbolKind.Namespace => 3
    case SymbolKind.Package => 4
    case SymbolKind.Class => 5
    case SymbolKind.Method => 6
    case SymbolKind.Property => 7
    case SymbolKind.Field => 8
    case SymbolKind.Constructor => 9
    case SymbolKind.Enum => 10
    case SymbolKind.Interface => 11
    case SymbolKind.Function => 12
    case SymbolKind.Variable => 13
    case SymbolKind.Constant => 14
    case SymbolKind.String => 15
    case SymbolKind.Number => 16
    case SymbolKind.Boolean => 17
    case SymbolKind.Array => 18
    case SymbolKind.Object => 19
    case SymbolKind.Key => 20
    case SymbolKind.Null => 21
    case SymbolKind.EnumMember => 22
    case SymbolKind.Struct => 23
    case SymbolKind.Event => 24
    case SymbolKind.Operator => 25
    case SymbolKind.TypeParameter => 26
  }
}

object SymbolKind {

  case object File extends SymbolKind

  case object Module extends SymbolKind

  case object Namespace extends SymbolKind

  case object Package extends SymbolKind

  case object Class extends SymbolKind

  case object Method extends SymbolKind

  case object Property extends SymbolKind

  case object Field extends SymbolKind

  case object Constructor extends SymbolKind

  case object Enum extends SymbolKind

  case object Interface extends SymbolKind

  case object Function extends SymbolKind

  case object Variable extends SymbolKind

  case object Constant extends SymbolKind

  case object String extends SymbolKind

  case object Number extends SymbolKind

  case object Boolean extends SymbolKind

  case object Array extends SymbolKind

  case object Object extends SymbolKind

  case object Key extends SymbolKind

  case object Null extends SymbolKind

  case object EnumMember extends SymbolKind

  case object Struct extends SymbolKind

  case object Event extends SymbolKind

  case object Operator extends SymbolKind

  case object TypeParameter extends SymbolKind

}
