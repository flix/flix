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
  * Every name that stands for a declaration keeps its [[Symbol]]: in a [[RichText.SymDecl]] where the
  * declaration is made, and in a [[RichText.SymUse]] everywhere else. Every other colored atom carries
  * the [[SemanticTokenType]] the LSP assigns to the same syntax in a [[RichText.Token]]. Punctuation is
  * [[RichText.Text]] and is never styled.
  */
sealed trait RichText {

  /**
    * Returns `this` followed by `that`.
    */
  def ++(that: RichText): RichText = RichText.Concat(List(this, that))

  /**
    * Renders this tree as plain text.
    */
  def plain: String = RichText.render(this, PlainEmitter)

  /**
    * Renders this tree as text for a terminal: every colored atom in the color of its kind, as
    * `fmt` writes it.
    */
  def ansi(fmt: Formatter): String = RichText.render(this, new AnsiEmitter(fmt))

  /**
    * Renders this tree as HTML.
    *
    * A [[RichText.SymUse]] whose symbol `link` resolves becomes an anchor to that URL; every other
    * colored atom, a [[RichText.SymDecl]] included, becomes a span with the class `cssClass` gives its
    * kind, or plain text if it gives none. The text is escaped.
    */
  def html(link: Symbol => Option[String], cssClass: SemanticTokenType => Option[String]): String =
    RichText.render(this, new HtmlEmitter(link, cssClass))

  /**
    * Returns the colored atoms of this tree in order, without the punctuation between them.
    */
  def tokens: List[(SemanticTokenType, String)] = RichText.render(this, TokenEmitter)

}

object RichText {

  /**
    * Punctuation and spacing that carries no meaning of its own: `(`, `, `, `: `, `[`, `{ `, ` | `.
    */
  case class Text(s: String) extends RichText

  /**
    * A colored atom that stands for no declaration: a keyword, an operator, a primitive type, a
    * type variable.
    */
  case class Token(kind: SemanticTokenType, s: String) extends RichText

  /**
    * The declaration of a symbol: the name of a declaration where it is declared, e.g. `sort` in
    * `def sort(l: List[a]): List[a]`. It is colored like a use of the symbol, but is never linked.
    */
  case class SymDecl(sym: Symbol, kind: SemanticTokenType, s: String) extends RichText

  /**
    * A use of a symbol: a name that stands for a declaration made elsewhere and can be linked to
    * it. `s` is the text as printed, qualified or not.
    */
  case class SymUse(sym: Symbol, kind: SemanticTokenType, s: String) extends RichText

  /**
    * Its parts, in order.
    */
  case class Concat(parts: List[RichText]) extends RichText

  /**
    * The empty tree.
    */
  val Empty: RichText = Concat(Nil)

  /**
    * Returns the given punctuation or spacing.
    */
  def text(s: String): RichText = Text(s)

  /**
    * Returns the given keyword, e.g. `def`, `with`, `not`.
    */
  def kw(s: String): RichText = Token(SemanticTokenType.Keyword, s)

  /**
    * Returns the given operator, e.g. `->`, `\`, `+`.
    */
  def op(s: String): RichText = Token(SemanticTokenType.Operator, s)

  /**
    * Returns the given built-in type, e.g. `Int32`, `Array`, or kind.
    */
  def ty(s: String): RichText = Token(SemanticTokenType.Type, s)

  /**
    * Returns the given built-in effect, e.g. `{}`, `Univ`.
    */
  def ef(s: String): RichText = Token(SemanticTokenType.Effect, s)

  /**
    * Returns the given type or region variable, e.g. `a`, `ef`, `r`.
    */
  def tparam(s: String): RichText = Token(SemanticTokenType.TypeParameter, s)

  /**
    * Returns the given record label, struct field or predicate name.
    */
  def prop(s: String): RichText = Token(SemanticTokenType.Property, s)

  /**
    * Returns `fs` separated by the punctuation `s`.
    */
  def sep(s: String, fs: List[RichText]): RichText = Concat(fs.flatMap(f => List(Text(s), f)).drop(1))

  /**
    * Returns `fs` separated by the tree `s`, e.g. a keyword with a space on either side.
    */
  def sep(s: RichText, fs: List[RichText]): RichText = Concat(fs.flatMap(f => List(s, f)).drop(1))

  /**
    * Returns `f` in parentheses.
    */
  def parens(f: RichText): RichText = Concat(List(Text("("), f, Text(")")))

  /**
    * Folds the tree `f` with the emitter `e`.
    */
  def render[A](f: RichText, e: Emitter[A]): A = f match {
    case Text(s) => e.text(s)
    case Token(kind, s) => e.token(kind, s)
    case SymDecl(sym, kind, s) => e.symDecl(sym, kind, s)
    case SymUse(sym, kind, s) => e.symUse(sym, kind, s)
    case Concat(parts) => e.concat(parts.map(render(_, e)))
  }

}
