/*
 * Copyright 2021 Jacob Harris Cryer Kragh
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

/**
  * Represents a semantic token type in LSP.
  *
  * Note: The intermediate TypeScript server is responsible for communicating the legend
  * to the client. The TS server must list the token types in the same order as is used
  * here in the Scala code. The reason for this is that the response is encoded as a
  * sequence of integers, and the integer representing a token type is the type's
  * index in the legend.
  *
  * NB: Must be in sync with server\src\handlers.ts
  */
sealed trait SemanticTokenType {

  def toInt: Int = this match {
    case SemanticTokenType.Namespace => 0
    case SemanticTokenType.Type => 1
    case SemanticTokenType.Class => 2
    case SemanticTokenType.Enum => 3
    case SemanticTokenType.Interface => 4
    case SemanticTokenType.Struct => 5
    case SemanticTokenType.TypeParameter => 6
    case SemanticTokenType.Parameter => 7
    case SemanticTokenType.Variable => 8
    case SemanticTokenType.Property => 9
    case SemanticTokenType.EnumMember => 10
    case SemanticTokenType.Event => 11
    case SemanticTokenType.Function => 12
    case SemanticTokenType.Method => 13
    case SemanticTokenType.Macro => 14
    case SemanticTokenType.Keyword => 15
    case SemanticTokenType.Modifier => 16
    case SemanticTokenType.Comment => 17
    case SemanticTokenType.String => 18
    case SemanticTokenType.Number => 19
    case SemanticTokenType.Regexp => 20
    case SemanticTokenType.Operator => 21
    case SemanticTokenType.Decorator => 22
  }
}

object SemanticTokenType {

  /**
    * For identifiers that declare or reference a namespace, module, or package.
    */
  case object Namespace extends SemanticTokenType

  /**
    * For identifiers that declare or reference a type that is not covered elsewhere.
    */
  case object Type extends SemanticTokenType

  /**
    * For identifiers that declare or reference a class type.
    */
  case object Class extends SemanticTokenType

  /**
    * For identifiers that declare or reference an enumeration type.
    */
  case object Enum extends SemanticTokenType

  /**
    * For identifiers that declare or reference an interface type.
    */
  case object Interface extends SemanticTokenType

  /**
    * For identifiers that declare or reference a struct type.
    */
  case object Struct extends SemanticTokenType

  /**
    * For identifiers that declare or reference a type parameter.
    */
  case object TypeParameter extends SemanticTokenType

  /**
    * For identifiers that declare or reference a function or method parameters.
    */
  case object Parameter extends SemanticTokenType

  /**
    * For identifiers that declare or reference a local or global variable.
    */
  case object Variable extends SemanticTokenType

  /**
    * For identifiers that declare or reference a member property, member field, or member variable.
    */
  case object Property extends SemanticTokenType

  /**
    * For identifiers that declare an enumeration property, constant, or member.
    */
  case object EnumMember extends SemanticTokenType

  /**
    * For identifiers that declare or reference an event.
    */
  case object Event extends SemanticTokenType

  /**
    * For identifiers that declare a function.
    */
  case object Function extends SemanticTokenType

  /**
    * For identifiers that declare a member function or method.
    */
  case object Method extends SemanticTokenType

  /**
    * For identifiers that declare or reference a macro.
    */
  case object Macro extends SemanticTokenType

  /**
    * For tokens that represent a keyword.
    */
  case object Keyword extends SemanticTokenType

  /**
    * For tokens that represent a modifier.
    */
  case object Modifier extends SemanticTokenType

  /**
    * For tokens that represent a comment.
    */
  case object Comment extends SemanticTokenType

  /**
    * For tokens that represent a string.
    */
  case object String extends SemanticTokenType

  /**
    * For tokens that represent a number.
    */
  case object Number extends SemanticTokenType

  /**
    * For tokens that represent a regular expression.
    */
  case object Regexp extends SemanticTokenType

  /**
    * For tokens that represent an operator.
    */
  case object Operator extends SemanticTokenType

  /**
    * For tokens that represent a decorator.
    */
  case object Decorator extends SemanticTokenType

  def getWholeList: List[String] = List(
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "method",
    "macro",
    "keyword",
    "modifier",
    "comment",
    "string",
    "number",
    "regexp",
    "operator",
    "decorator"
  )
}
