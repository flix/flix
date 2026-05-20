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
 * Represents a semantic token modifier in LSP.
 *
 * Note: The intermediate TypeScript server is responsible for communicating the legend
 * to the client. The TS server must list the token modifiers in the same order as is used
 * here in the Scala code. The reason for this is that the response is encoded as a
 * sequence of integers, and a modifier is represented by its index in the legend.
 */
sealed trait SemanticTokenModifier {
  def toInt: Int = this match {
    case SemanticTokenModifier.Declaration => 0
    case SemanticTokenModifier.Definition => 1
    case SemanticTokenModifier.Readonly => 2
    case SemanticTokenModifier.Static => 3
    case SemanticTokenModifier.Deprecated => 4
    case SemanticTokenModifier.Abstract => 5
    case SemanticTokenModifier.Async => 6
    case SemanticTokenModifier.Modification => 7
    case SemanticTokenModifier.Documentation => 8
    case SemanticTokenModifier.DefaultLibrary => 9
  }
}

object SemanticTokenModifier {
  case object Declaration extends SemanticTokenModifier

  case object Definition extends SemanticTokenModifier

  case object Readonly extends SemanticTokenModifier

  case object Static extends SemanticTokenModifier

  case object Deprecated extends SemanticTokenModifier

  case object Abstract extends SemanticTokenModifier

  case object Async extends SemanticTokenModifier

  case object Modification extends SemanticTokenModifier

  case object Documentation extends SemanticTokenModifier

  case object DefaultLibrary extends SemanticTokenModifier

  def getWholeList: List[String] = List(
    "declaration",
    "definition",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary"
  )
}
