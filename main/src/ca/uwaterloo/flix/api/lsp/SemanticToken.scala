/*
 * Copyright 2021 Jacob Harris Cryer Kragh
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Represents a semantic token in LSP.
  *
  * @param tpe the token type (e.g. variable).
  * @param mod the token modifiers (e.g. static).
  * @param loc the source location of the semantic token.
  */
case class SemanticToken(tpe: SemanticTokenType, mod: List[SemanticTokenModifier], loc: SourceLocation)
