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
 */
sealed trait SemanticTokenType {
  def toInt: Int = this match {
    case SemanticTokenType.Number => 0
    case SemanticTokenType.Str => 1
  }
}

object SemanticTokenType {
  case object Number extends SemanticTokenType

  case object Str extends SemanticTokenType
}
