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
  * The layout mode under which a [[Doc]] is rendered.
  *
  * flixfmt does not compute layout decisions based on a width constraint.
  */
sealed trait Layout
object Layout {
  case object SingleLine extends Layout
  case object MultiLine extends Layout
}

/**
  * A `Doc` is the algebraic representation of a formatted document.
  *
  *   - `<>`  concatenation of two documents with nothing in between.
  *   - `<+>` concatenation with a space.
  *   - `<|>` concatenation with a soft line break.
  */
sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
  def <+>(right: Doc): Doc = this <> Doc.space <> right
  def <|>(right: Doc): Doc = this <> Doc.line <> right
}

/**
  * Constructors, smart constructors, derived combinators and the [[pretty]]
  * rendering function for the [[Doc]] algebra.
  *
  *   1. Wadler core the basic compositional combinators.
  *   2. Leijen position-aware extensions.
  *   3. Layout-mode constructors, layout invariant.
  *   4. Fixed line break.
  */
object Doc {

  /** The empty document; the identity for [[Concat]]. */
  case object Empty extends Doc

  /** A literal string. */
  case class Text(str: String) extends Doc

  /** Concatenation of `left` followed by `right`. */
  case class Concat(left: Doc, right: Doc) extends Doc

  /**
    * A soft line break: a single space in `SingleLine` mode, and a newline followed by the prevailing indentation in `MultiLine` mode.
    */
  case object Line extends Doc

  /**
    * `doc` rendered with the indentation level increased by `level` columns.
    */
  case class Nest(level: Int, doc: Doc) extends Doc

  /**
    * `doc` rendered with its indentation level set to the *current* output column rather than the lexical nesting level.
    */
  private case class Align(doc: Doc) extends Doc

  /**
    * A document whose content is determined by the current column,
    * `f` is  applied to the renderer's current column position and the resulting [[Doc]] is rendered in its place.
    */
  private case class Column(f: Int => Doc) extends Doc

  /**
    * `doc` rendered with its indentation level set to the *absolute* column
    * `level`, regardless of the surrounding nesting context.
    */
  private case class NestAbsolute(level: Int, doc: Doc) extends Doc

  /**
    * Two alternative documents: `singleLine` is selected when the prevailing
    * layout mode is `SingleLine`, `multiLine` when it is `MultiLine`.
    */
  case class LayoutChoice(singleLine: Doc, multiLine: Doc) extends Doc

  /**
    * `doc` rendered under the fixed layout mode `layout`, regardless of the surrounding layout.
    */
  case class SetLayout(layout: Layout, doc: Doc) extends Doc

  /** A line break that produces a newline regardless of the layout mode. */
  case object HardLine extends Doc


  def text(str: String): Doc = Text(str)
  def line: Doc = Line
  def hardline: Doc = HardLine
  def space: Doc = Text(" ")
  def empty: Doc = Empty
  def nest(level: Int, doc: Doc): Doc = Nest(level, doc)
  def nestAbsolute(level: Int, doc: Doc): Doc = NestAbsolute(level, doc)
  def align(doc: Doc): Doc = Align(doc)
  def column(f: Int => Doc): Doc = Column(f)
  def setLayout(layout: Layout, doc: Doc): Doc = SetLayout(layout, doc)
  def layoutChoice(singleLine: Doc, multiLine: Doc): Doc = LayoutChoice(singleLine, multiLine)

  /**
    * Renders `doc` and, in `MultiLine` mode, pads it with spaces so the content occupies at least `width` columns.
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
    case Nil      => Empty
    case x :: Nil => x
    case x :: xs  => f(x, folddoc(f, xs))
  }

  /**
    * Renders `doc` to a string using the default layout mode `MultiLine`.
    */
  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, Layout.MultiLine, doc)))
    sb.toString()
  }

  /**
    * Renders `doc` to a string under the given top-level `layout` mode.
    */
  def pretty(layout: Layout, doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, layout, doc)))
    sb.toString()
  }

  /** Trims trailing spaces from the end of the buffer. Avoids unnecessary whitespaces at the end. */
  private def trimTrailing(sb: StringBuilder): Unit = {
    var i = sb.length - 1
    while (i >= 0 && sb.charAt(i) == ' ') i -= 1
    sb.setLength(i + 1)
  }

  /**
    * The rendering algorithm: a single, tail-recursive pass over the document
    * tree.
    **
    * At each step the head of the worklist is consumed and dispatched on by
    * constructor. There is no backtracking, no lookahead and no width
    * decision as with Wadler's algorithm. All layout decisions have already been
    * encoded into the document via [[SetLayout]] / [[LayoutChoice]].
    *
    * @param sb   the StringBuilder accumulating the output
    * @param k    the current output column
    * @param docs the worklist of `(indentation, layout, document)` triples
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
        case Layout.MultiLine  => render(sb, k, (i + j, l, x) :: z)
      }

    case (i, l, Align(x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine  => render(sb, k, (k, l, x) :: z)
      }

    case (i, l, Column(f)) :: z =>
      render(sb, k, (i, l, f(k)) :: z)

    case (i, l, LayoutChoice(s, m)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, s) :: z)
        case Layout.MultiLine  => render(sb, k, (i, l, m) :: z)
      }

    case (i, _, SetLayout(newLayout, x)) :: z =>
      render(sb, k, (i, newLayout, x) :: z)

    case (_, l, NestAbsolute(j, x)) :: z =>
      render(sb, k, (j, l, x) :: z)
  }
}
