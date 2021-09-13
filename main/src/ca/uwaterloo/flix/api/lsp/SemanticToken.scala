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

import ca.uwaterloo.flix.api.lsp.SemanticTokenModifier.SemanticTokenModifier
import ca.uwaterloo.flix.api.lsp.SemanticTokenType.SemanticTokenType
import ca.uwaterloo.flix.language.ast.SourceLocation

/**
 * Represents a semantic token in LSP.
 *
 * @param loc The location of the token.
 * @param tokenType What kind of token this is ("number" or "string" or "function" or ...).
 * @param tokenModifiers A list of modifiers that apply to this token (e.g. "declaration" or "static").
 */
case class SemanticToken(loc: SourceLocation, tokenType: SemanticTokenType, tokenModifiers: List[SemanticTokenModifier])
