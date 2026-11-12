/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.Formatter

/**
  * Printed Flix syntax, annotated for an [[Emitter]].
  *
  * A printer builds the tree once. An emitter then renders it as plain text, as text colored for a
  * terminal, as HTML with a link per declaration and a class per token, or as a list of tokens,
  * without the printer knowing which. The tree is immutable and cheap, so one signature can be rendered in more than one way
  * without being printed twice.
  *
  * Every name that stands for a declaration keeps its [[Symbol]] in a [[Fmt.SymUse]], and every
  * other colored atom carries the [[SemanticTokenType]] the LSP assigns to the same syntax in a
  * [[Fmt.Token]]. Punctuation is [[Fmt.Text]] and is never styled.
  */
sealed trait Fmt {

  /**
    * Returns `this` followed by `that`.
    */
  def ++(that: Fmt): Fmt = Fmt.Concat(List(this, that))

  /**
    * Renders this tree as plain text.
    */
  def plain: String = Fmt.render(this, PlainEmitter)

  /**
    * Renders this tree as text for a terminal: every colored atom in the color of its kind, as
    * `fmt` writes it.
    */
  def ansi(fmt: Formatter): String = Fmt.render(this, new AnsiEmitter(fmt))

  /**
    * Renders this tree as HTML.
    *
    * A [[Fmt.SymUse]] whose symbol `link` resolves becomes an anchor to that URL; every other
    * colored atom becomes a span with the class `cssClass` gives its kind, or plain text if it
    * gives none. The text is escaped.
    */
  def html(link: Symbol => Option[String], cssClass: SemanticTokenType => Option[String]): String =
    Fmt.render(this, new HtmlEmitter(link, cssClass))

  /**
    * Returns the colored atoms of this tree in order, without the punctuation between them.
    */
  def tokens: List[(SemanticTokenType, String)] = Fmt.render(this, TokenEmitter)

}

object Fmt {

  /**
    * Punctuation and spacing that carries no meaning of its own: `(`, `, `, `: `, `[`, `{ `, ` | `.
    */
  case class Text(s: String) extends Fmt

  /**
    * A colored atom that stands for no declaration: a keyword, an operator, a primitive type, a
    * type variable.
    */
  case class Token(kind: SemanticTokenType, s: String) extends Fmt

  /**
    * A use of a symbol: a name that stands for a declaration and can be linked. `s` is the text as
    * printed, qualified or not.
    */
  case class SymUse(sym: Symbol, kind: SemanticTokenType, s: String) extends Fmt

  /**
    * Its parts, in order.
    */
  case class Concat(parts: List[Fmt]) extends Fmt

  /**
    * The empty tree.
    */
  val Empty: Fmt = Concat(Nil)

  /**
    * Returns the given punctuation or spacing.
    */
  def text(s: String): Fmt = Text(s)

  /**
    * Returns the given keyword, e.g. `def`, `with`, `not`.
    */
  def kw(s: String): Fmt = Token(SemanticTokenType.Keyword, s)

  /**
    * Returns the given operator, e.g. `->`, `\`, `+`.
    */
  def op(s: String): Fmt = Token(SemanticTokenType.Operator, s)

  /**
    * Returns the given built-in type, e.g. `Int32`, `Array`, or kind.
    */
  def ty(s: String): Fmt = Token(SemanticTokenType.Type, s)

  /**
    * Returns the given built-in effect, e.g. `{}`, `Univ`.
    */
  def ef(s: String): Fmt = Token(SemanticTokenType.Effect, s)

  /**
    * Returns the given type or region variable, e.g. `a`, `ef`, `r`.
    */
  def tparam(s: String): Fmt = Token(SemanticTokenType.TypeParameter, s)

  /**
    * Returns the given record label, struct field or predicate name.
    */
  def prop(s: String): Fmt = Token(SemanticTokenType.Property, s)

  /**
    * Returns `fs` separated by the punctuation `s`.
    */
  def sep(s: String, fs: List[Fmt]): Fmt = Concat(fs.flatMap(f => List(Text(s), f)).drop(1))

  /**
    * Returns `fs` separated by the tree `s`, e.g. a keyword with a space on either side.
    */
  def sep(s: Fmt, fs: List[Fmt]): Fmt = Concat(fs.flatMap(f => List(s, f)).drop(1))

  /**
    * Returns `f` in parentheses.
    */
  def parens(f: Fmt): Fmt = Concat(List(Text("("), f, Text(")")))

  /**
    * Folds the tree `f` with the emitter `e`.
    */
  def render[A](f: Fmt, e: Emitter[A]): A = f match {
    case Text(s) => e.text(s)
    case Token(kind, s) => e.token(kind, s)
    case SymUse(sym, kind, s) => e.symUse(sym, kind, s)
    case Concat(parts) => e.concat(parts.map(render(_, e)))
  }

}
