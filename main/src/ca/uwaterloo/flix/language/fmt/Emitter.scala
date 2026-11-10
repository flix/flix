/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.Symbol

/**
  * A fold over [[Fmt]]: one method per constructor. `A` is what the emitter builds: a string, a
  * token list, ...
  *
  * A printer never knows which emitter runs, and an emitter never knows what it prints.
  */
trait Emitter[A] {

  /**
    * Punctuation or spacing, see [[Fmt.Text]].
    */
  def text(s: String): A

  /**
    * A colored atom that stands for no declaration, see [[Fmt.Token]].
    */
  def token(kind: SemanticTokenType, s: String): A

  /**
    * A name that stands for a declaration, see [[Fmt.Ref]].
    */
  def ref(sym: Symbol, kind: SemanticTokenType, s: String): A

  /**
    * The parts of a [[Fmt.Concat]], already emitted, in order.
    */
  def concat(parts: List[A]): A

}

/**
  * Plain text: every atom is written as it is, without color or links.
  */
object PlainEmitter extends Emitter[String] {

  def text(s: String): String = s

  def token(kind: SemanticTokenType, s: String): String = s

  def ref(sym: Symbol, kind: SemanticTokenType, s: String): String = s

  def concat(parts: List[String]): String = parts.mkString

}

/**
  * The token list: what tests assert on, and what a client that wants to re-color starts from.
  */
object TokenEmitter extends Emitter[List[(SemanticTokenType, String)]] {

  def text(s: String): List[(SemanticTokenType, String)] = Nil

  def token(kind: SemanticTokenType, s: String): List[(SemanticTokenType, String)] = List((kind, s))

  def ref(sym: Symbol, kind: SemanticTokenType, s: String): List[(SemanticTokenType, String)] = List((kind, s))

  def concat(parts: List[List[(SemanticTokenType, String)]]): List[(SemanticTokenType, String)] = parts.flatten

}
