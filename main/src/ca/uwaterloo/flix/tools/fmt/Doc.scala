/*
 * Copyright 2026 Din Jakupi
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
package ca.uwaterloo.flix.tools.fmt

import scala.annotation.tailrec

/**
  * A `Doc` is an abstract, composable description of a formatted document.
  *
  * You build a `Doc` from the combinators and build a string from a `Doc` by calling [[pretty]].
  *
  * Unlike the Wadler and Leijen pretty-printers it is derived from, flixfmt makes
  * no layout decision from a width constraint. Every choice between a
  * single-line and a multi-line rendering is encoded explicitly in the
  * document itself via [[Doc.Layout]], [[Doc.LayoutChoice]] and
  * [[Doc.SetLayout]]. This means that the rendering algorithm is a single pass.
  */
sealed trait Doc {

  /** Concatenates `this` and `right` with nothing in between. */
  def <>(right: Doc): Doc = Doc.Concat(this, right)

  /** Concatenates `this` and `right` separated by a single space. */
  def <+>(right: Doc): Doc = this <> Doc.space <> right

  /** Concatenates `this` and `right` separated by a soft line break ([[Doc.line]]). */
  def <|>(right: Doc): Doc = this <> Doc.line <> right
}

/**
  * The [[Doc]] algebra: its constructors, smart constructors, derived
  * combinators and the [[pretty]] renderer.
  *
  * The constructors form four layers:
  *
  *   1. Wadler core           — [[Empty]], [[Text]], [[Concat]], [[Line]], [[Nest]].
  *   2. Leijen position-aware — [[Align]], [[Column]], [[NestAbsolute]].
  *   3. Layout-mode           — [[LayoutChoice]], [[SetLayout]] (see [[Layout]]).
  *   4. Fixed break           — [[HardLine]]
  *
  * References:
  *
  *   - Wadler, "A prettier printer":
  *     https://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf
  *   - Leijen, the `pprint` library (position aware extensions):
  *     https://github.com/haskell-prettyprinter/prettyprinter
  *     https://web.archive.org/web/20060620042006/http://www.cs.uu.nl/~daan/download/pprint/pprint.html
  *   - Lyngby and Due, "A Formatter for Futhark" (layout mode approach without a width constraint):
  *     https://futhark-lang.org/student-projects/therese-william-project.pdf
  */
object Doc {

  /**
    * The mode under which a [[Doc]] is rendered.
    *
    * flixfmt never derives this from a width constraint: the mode is fixed by
    * [[SetLayout]] and chosen between by [[LayoutChoice]]. In `SingleLine`
    * mode every [[Line]] is a space and in `MultiLine` mode every [[Line]] is a
    * newline followed by the prevailing indentation.
    */
  sealed trait Layout

  object Layout {
    case object SingleLine extends Layout

    case object MultiLine extends Layout
  }

  /** The empty document; the identity for [[Concat]] (see [[empty]]). */
  case object Empty extends Doc

  /** The literal string `str` (see [[text]]). */
  case class Text(str: String) extends Doc

  /** `left` immediately followed by `right` (see the [[Doc.<>]] operator). */
  case class Concat(left: Doc, right: Doc) extends Doc

  /** A soft line break (see [[line]]). */
  case object Line extends Doc

  /** `doc` indented by `level` extra columns (see [[nest]]). */
  case class Nest(level: Int, doc: Doc) extends Doc

  /** `doc` indented to the current output column (see [[align]]). */
  private case class Align(doc: Doc) extends Doc

  /** Content computed from the current output column (see [[column]]). */
  private case class Column(f: Int => Doc) extends Doc

  /** `doc` indented to the absolute column `level` (see [[nestAbsolute]]). */
  private case class NestAbsolute(level: Int, doc: Doc) extends Doc

  /** Picks `singleLine` or `multiLine` by layout mode (see [[layoutChoice]]). */
  case class LayoutChoice(singleLine: Doc, multiLine: Doc) extends Doc

  /** `doc` rendered under a fixed layout mode (see [[setLayout]]). */
  case class SetLayout(layout: Layout, doc: Doc) extends Doc

  /** A hard line break (see [[hardline]]). */
  case object HardLine extends Doc

  /**
    * A document consisting of the literal string `str`.
    *
    * `str` should not contain newlines; use [[line]] or [[hardline]] for
    * breaks so that indentation is tracked correctly.
    */
  def text(str: String): Doc = Text(str)

  /**
    * A soft line break: a single space in `SingleLine` mode, and a newline
    * followed by the prevailing indentation in `MultiLine` mode.
    */
  def line: Doc = Line

  /** A line break that is always a newline plus indentation, regardless of layout mode. */
  def hardline: Doc = HardLine

  /** A single space. */
  def space: Doc = Text(" ")

  /** The empty document; the identity for [[Doc.Concat]]. */
  def empty: Doc = Empty

  /** `doc` with the indentation level increased by `level` columns (in `MultiLine` mode). */
  def nest(level: Int, doc: Doc): Doc = Nest(level, doc)

  /**
    * `doc` with its indentation level set to the absolute column `level`,
    * regardless of the surrounding nesting context.
    */
  def nestAbsolute(level: Int, doc: Doc): Doc = NestAbsolute(level, doc)

  /**
    * `doc` with its indentation level set to the *current* output column
    * rather than the lexical nesting level.
    */
  def align(doc: Doc): Doc = Align(doc)

  /**
    * A document whose content depends on the current output column: `f` is
    * applied to that column and the resulting [[Doc]] is rendered in its place.
    */
  def column(f: Int => Doc): Doc = Column(f)

  /** `doc` rendered under the fixed layout mode `layout`, regardless of the surrounding layout. */
  def setLayout(layout: Layout, doc: Doc): Doc = SetLayout(layout, doc)

  /** Renders `singleLine` in `SingleLine` mode and `multiLine` in `MultiLine` mode. */
  def layoutChoice(singleLine: Doc, multiLine: Doc): Doc = LayoutChoice(singleLine, multiLine)

  /**
    * Renders `doc` and, in `MultiLine` mode, pads it with spaces so the
    * content occupies at least `width` columns.
    *
    * see https://hackage.haskell.org/package/ansi-wl-pprint-0.6.9/docs/Text-PrettyPrint-ANSI-Leijen.html
    */
  def fill(width: Int, doc: Doc): Doc =
    layoutChoice(
      doc,
      column(k1 => doc <> column(k2 => {
        val used = k2 - k1
        if (used >= width) empty else text(" " * (width - used))
      }))
    )

  /** Folds `docs` with `<|>`, separating each pair by a soft line break. */
  def stack(docs: List[Doc]): Doc = folddoc(_ <|> _, docs)

  /** Folds `docs`, separating each pair by a fixed newline ([[hardline]]). */
  def hardStack(docs: List[Doc]): Doc = folddoc((a, b) => a <> hardline <> b, docs)

  /** Folds `docs`, separating each pair by an arbitrary `separator` document. */
  def sep(separator: Doc, docs: List[Doc]): Doc = folddoc((a, b) => a <> separator <> b, docs)

  /** Right fold over `docs` with `f`. */
  private def folddoc(f: (Doc, Doc) => Doc, docs: List[Doc]): Doc = docs match {
    case Nil => Empty
    case x :: Nil => x
    case x :: xs => f(x, folddoc(f, xs))
  }

  /** Renders `doc` to a string using the default layout mode `MultiLine`. */
  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, Layout.MultiLine, doc)))
    sb.toString()
  }

  /** Renders `doc` to a string under the given top-level `layout` mode. */
  def pretty(layout: Layout, doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, layout, doc)))
    sb.toString()
  }

  /** Trims trailing spaces from the end of the buffer, avoiding trailing whitespace on a line. */
  private def trimTrailing(sb: StringBuilder): Unit = {
    var i = sb.length - 1
    while (i >= 0 && sb.charAt(i) == ' ') i -= 1
    sb.setLength(i + 1)
  }

  /**
    * The rendering algorithm: a single, tail-recursive pass over the document tree.
    *
    * At each step the head of the worklist is consumed and dispatched on by
    * constructor. There is no backtracking, no lookahead and no width
    * decision as in Wadler's algorithm: all layout decisions have already been
    * encoded into the document via [[SetLayout]] and [[LayoutChoice]].
    *
    * @param sb   the StringBuilder accumulating the output
    * @param k    the current output column
    * @param docs the worklist of `(indentation, layout, document)` tuples to process
    */
  @tailrec
  private def render(sb: StringBuilder, k: Int, docs: List[(Int, Layout, Doc)]): Unit = docs match {
    case Nil => ()

    case (_, _, Empty) :: z =>
      render(sb, k, z)

    case (i, l, Concat(x, y)) :: z =>
      render(sb, k, (i, l, x) :: (i, l, y) :: z)

    case (_, _, Text(s)) :: z =>
      sb.append(s)
      render(sb, k + s.length, z)

    case (i, l, Line) :: z =>
      l match {
        case Layout.SingleLine =>
          sb.append(' ')
          render(sb, k + 1, z)
        case Layout.MultiLine =>
          trimTrailing(sb)
          sb.append('\n')
          sb.append(" " * i)
          render(sb, i, z)
      }

    case (i, _, HardLine) :: z =>
      trimTrailing(sb)
      sb.append('\n')
      sb.append(" " * i)
      render(sb, i, z)

    case (i, l, Nest(j, x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine => render(sb, k, (i + j, l, x) :: z)
      }

    case (i, l, Align(x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine => render(sb, k, (k, l, x) :: z)
      }

    case (i, l, Column(f)) :: z =>
      render(sb, k, (i, l, f(k)) :: z)

    case (i, l, LayoutChoice(s, m)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, s) :: z)
        case Layout.MultiLine => render(sb, k, (i, l, m) :: z)
      }

    case (i, _, SetLayout(newLayout, x)) :: z =>
      render(sb, k, (i, newLayout, x) :: z)

    case (_, l, NestAbsolute(j, x)) :: z =>
      render(sb, k, (j, l, x) :: z)
  }
}
