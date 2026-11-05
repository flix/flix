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
  * Plain text: `toString`, error messages, hovers, completions and tests.
  */
object PlainEmitter extends Emitter[String] {

  def text(s: String): String = s

  def token(kind: SemanticTokenType, s: String): String = s

  def ref(sym: Symbol, kind: SemanticTokenType, s: String): String = s

  def concat(parts: List[String]): String = parts.mkString

}

/**
  * HTML: a span per token, an anchor per resolvable reference, escaped text.
  *
  * `link` gives the URL of the page or anchor that documents a symbol, or `None` if there is none,
  * in which case the name is a colored token like any other rather than a dead link. `cssClass`
  * gives the class of a kind of token, or `None` if it is not styled. The emitter knows neither:
  * the documentor knows which pages it writes and which stylesheet they load.
  */
class HtmlEmitter(link: Symbol => Option[String], cssClass: SemanticTokenType => Option[String]) extends Emitter[String] {

  def text(s: String): String = HtmlEmitter.escape(s)

  def token(kind: SemanticTokenType, s: String): String = cssClass(kind) match {
    case Some(cls) => s"<span class='$cls'>${HtmlEmitter.escape(s)}</span>"
    case None => HtmlEmitter.escape(s)
  }

  def ref(sym: Symbol, kind: SemanticTokenType, s: String): String = link(sym) match {
    case Some(url) =>
      val cls = cssClass(kind).map(c => s" class='$c'").getOrElse("")
      s"<a$cls href='${HtmlEmitter.escape(url)}'>${HtmlEmitter.escape(s)}</a>"
    case None => token(kind, s)
  }

  def concat(parts: List[String]): String = parts.mkString

}

object HtmlEmitter {

  /**
    * Escapes `s` for use as text, or as a single- or double-quoted attribute value, in HTML.
    */
  def escape(s: String): String = {
    val sb = new StringBuilder(s.length)
    var i = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '&' => sb.append("&amp;")
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case '"' => sb.append("&quot;")
        case '\'' => sb.append("&#39;")
        case c => sb.append(c)
      }
      i += 1
    }
    sb.toString()
  }

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
