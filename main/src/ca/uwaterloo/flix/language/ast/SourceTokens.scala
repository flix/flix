/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

/**
  * The tokens of a single source.
  *
  * @param tokens         every token of the source, ending with [[TokenKind.Eof]].
  * @param semanticTokens the tokens in `tokens` that are semantic (see [[TokenKind.isSemantic]]), in
  *                       the same order. They are retained in the ASTs for the language server.
  */
case class SourceTokens(tokens: Array[Token], semanticTokens: Array[Token])
