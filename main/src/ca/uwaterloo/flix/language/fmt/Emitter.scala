/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.{Symbol, TypeConstructor}
import ca.uwaterloo.flix.util.Formatter

/**
  * A fold over [[RichText]]: one method per constructor. `A` is what the emitter builds: a string, a
  * token list, ...
  *
  * A printer never knows which emitter runs, and an emitter never knows what it prints.
  */
trait Emitter[A] {

  /**
    * Punctuation or spacing, see [[RichText.Text]].
    */
  def text(s: String): A

  /**
    * A colored atom that stands for no declaration, see [[RichText.Token]].
    */
  def token(kind: SemanticTokenType, s: String): A

  /**
    * A built-in type, see [[RichText.Builtin]].
    */
  def builtin(tc: TypeConstructor, s: String): A

  /**
    * The declaration of a symbol, i.e. the name of a declaration where it is declared, see
    * [[RichText.SymDecl]].
    */
  def symDecl(sym: Symbol, kind: SemanticTokenType, s: String): A

  /**
    * A use of a symbol, i.e. a name that stands for a declaration made elsewhere, see
    * [[RichText.SymUse]].
    */
  def symUse(sym: Symbol, kind: SemanticTokenType, s: String): A

  /**
    * The parts of a [[RichText.Concat]], already emitted, in order.
    */
  def concat(parts: List[A]): A

}

/**
  * Plain text: every atom is written as it is, without color or links.
  */
object PlainEmitter extends Emitter[String] {

  def text(s: String): String = s

  def token(kind: SemanticTokenType, s: String): String = s

  def builtin(tc: TypeConstructor, s: String): String = s

  def symDecl(sym: Symbol, kind: SemanticTokenType, s: String): String = s

  def symUse(sym: Symbol, kind: SemanticTokenType, s: String): String = s

  def concat(parts: List[String]): String = parts.mkString

}

/**
  * ANSI: every colored atom is written in the color of its kind, for a terminal.
  *
  * `fmt` writes the color, so a formatter without color support gives plain text. The colors are
  * those of the source code that an error message shows, so a type reads the same in both.
  */
class AnsiEmitter(fmt: Formatter) extends Emitter[String] {

  def text(s: String): String = s

  def token(kind: SemanticTokenType, s: String): String = AnsiEmitter.colorOf(kind) match {
    case Some((r, g, b)) => fmt.fgColor(r, g, b, s)
    case None => s
  }

  def builtin(tc: TypeConstructor, s: String): String = token(SemanticTokenType.Type, s)

  def symDecl(sym: Symbol, kind: SemanticTokenType, s: String): String = token(kind, s)

  def symUse(sym: Symbol, kind: SemanticTokenType, s: String): String = token(kind, s)

  def concat(parts: List[String]): String = parts.mkString

}

object AnsiEmitter {

  /**
    * Returns the RGB color of the given kind of token, if any.
    *
    * Uses Flixify Dark theme colors: https://github.com/flix/flixify-dark/blob/master/themes/flixify-dark.json
    */
  private def colorOf(kind: SemanticTokenType): Option[(Int, Int, Int)] = kind match {
    case SemanticTokenType.Keyword => Some((250, 62, 131))
    case SemanticTokenType.Modifier => Some((250, 62, 131))
    case SemanticTokenType.Comment => Some((140, 140, 140))
    case SemanticTokenType.String => Some((255, 238, 153))
    case SemanticTokenType.Number => Some((255, 140, 245))
    case SemanticTokenType.Regexp => Some((255, 238, 153))
    case SemanticTokenType.Function => Some((172, 227, 64))
    case SemanticTokenType.Method => Some((172, 227, 64))
    case SemanticTokenType.Type => Some((102, 217, 239))
    case SemanticTokenType.Enum => Some((102, 217, 239))
    case SemanticTokenType.Struct => Some((102, 217, 239))
    case SemanticTokenType.Interface => Some((102, 217, 239))
    case SemanticTokenType.Effect => Some((255, 140, 0))
    case SemanticTokenType.Class => Some((248, 248, 242))
    case SemanticTokenType.Variable => Some((200, 200, 194))
    case SemanticTokenType.Parameter => Some((200, 200, 194))
    case SemanticTokenType.Property => Some((200, 200, 194))
    case SemanticTokenType.TypeParameter => Some((248, 248, 242))
    case SemanticTokenType.EnumMember => Some((248, 248, 242))
    case _ => None
  }

}

/**
  * HTML: a span per token, an anchor per use of a symbol or built-in type that has a link, escaped
  * text.
  *
  * `link` gives the URL of the page or anchor that documents a symbol, and `builtinLink` that of
  * the page that documents a built-in type, or `None` if there is none, in which case the name is
  * a colored token like any other rather than a dead link. `cssClass` gives the class of a kind of
  * token, or `None` if it is not styled. The emitter knows none of them: its caller knows which
  * pages are written and which stylesheet they load.
  *
  * The declaration of a symbol is a colored token too: a declaration does not link to itself.
  */
class HtmlEmitter(link: Symbol => Option[String], builtinLink: TypeConstructor => Option[String], cssClass: SemanticTokenType => Option[String]) extends Emitter[String] {

  def text(s: String): String = HtmlEmitter.escape(s)

  def token(kind: SemanticTokenType, s: String): String = cssClass(kind) match {
    case Some(cls) => s"<span class='$cls'>${HtmlEmitter.escape(s)}</span>"
    case None => HtmlEmitter.escape(s)
  }

  def builtin(tc: TypeConstructor, s: String): String = anchor(builtinLink(tc), SemanticTokenType.Type, s)

  def symDecl(sym: Symbol, kind: SemanticTokenType, s: String): String = token(kind, s)

  def symUse(sym: Symbol, kind: SemanticTokenType, s: String): String = anchor(link(sym), kind, s)

  def concat(parts: List[String]): String = parts.mkString

  /**
    * Returns `s` as an anchor to `url`, if there is one, and as a token otherwise. Either has the
    * class of the given `kind`.
    */
  private def anchor(url: Option[String], kind: SemanticTokenType, s: String): String = url match {
    case Some(u) =>
      val cls = cssClass(kind) match {
        case Some(c) => s" class='$c'"
        case None => ""
      }
      s"<a$cls href='${HtmlEmitter.escape(u)}'>${HtmlEmitter.escape(s)}</a>"
    case None => token(kind, s)
  }

}

object HtmlEmitter {

  /**
    * Escapes `s` for use as text, or as a single- or double-quoted attribute value, in HTML.
    */
  private def escape(s: String): String = {
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

  def builtin(tc: TypeConstructor, s: String): List[(SemanticTokenType, String)] = List((SemanticTokenType.Type, s))

  def symDecl(sym: Symbol, kind: SemanticTokenType, s: String): List[(SemanticTokenType, String)] = List((kind, s))

  def symUse(sym: Symbol, kind: SemanticTokenType, s: String): List[(SemanticTokenType, String)] = List((kind, s))

  def concat(parts: List[List[(SemanticTokenType, String)]]): List[(SemanticTokenType, String)] = parts.flatten

}
