package ca.uwaterloo.flix.tools.fmt

import scala.annotation.tailrec

/**
  * The layout mode, determined by the original source code.
  * If the user wrote a term on a single line, it stays single-line.
  * If the user wrote it across multiple lines, it stays multi-line.
  */
sealed trait Layout
object Layout {
  case object SingleLine extends Layout
  case object MultiLine extends Layout
}

/**
  * A pretty-printing document following the Futhark/Ormolu approach:
  * layout-preserving rather than width-based.
  *
  * Key idea: instead of Wadler's `best`/`fits` choosing layouts based on
  * a page width, the layout (SingleLine vs MultiLine) is determined by the
  * original source code. The primitives `line` and `nest` behave differently
  * depending on the current layout mode:
  *
  *   - `line`:  SingleLine → space,  MultiLine → newline + indentation
  *   - `nest`:  SingleLine → no-op,  MultiLine → increase indentation
  *
  * The `<||>` operator provides explicit layout-dependent choice:
  * `s <||> m` renders `s` in SingleLine mode and `m` in MultiLine mode.
  */
sealed trait Doc {
  /** Concatenation. */
  def <>(right: Doc): Doc = Doc.Concat(this, right)

  /** Concatenation with a space in between. */
  def <+>(right: Doc): Doc = this <> Doc.space <> right

  /** Concatenation with a line in between (space or newline depending on layout). */
  def <|>(right: Doc): Doc = this <> Doc.line <> right

  /** Layout-dependent choice: `singleLineDoc <||> multiLineDoc`.
    * Renders the left in SingleLine mode, the right in MultiLine mode. */
  def <||>(right: Doc): Doc = Doc.LayoutChoice(this, right)
}

object Doc {

  // ── Document constructors ────────────────────────────────────────

  case object Empty extends Doc

  case class Text(str: String) extends Doc

  case class Concat(left: Doc, right: Doc) extends Doc

  /** A line break. In SingleLine layout this becomes a space.
    * In MultiLine layout this becomes a newline followed by the current indentation. */
  case class Line() extends Doc

  /** Increase indentation by `level` for the inner document.
    * In SingleLine layout this is a no-op (no indentation on a single line). */
  case class Nest(level: Int, doc: Doc) extends Doc

  /** Layout-dependent choice. Renders `singleLine` when the current layout
    * is SingleLine, and `multiLine` when the current layout is MultiLine.
    * This corresponds to Futhark's `<|>` operator. */
  case class LayoutChoice(singleLine: Doc, multiLine: Doc) extends Doc

  /** Set the layout mode for the inner document. This is how the formatter
    * communicates the original source layout into the document tree.
    * Analogous to Futhark's `local` that updates the Layout environment. */
  case class SetLayout(layout: Layout, doc: Doc) extends Doc

  // ── Smart constructors ───────────────────────────────────────────

  def text(str: String): Doc = Text(str)

  /** A line break: becomes a space in SingleLine, a newline in MultiLine. */
  def line: Doc = Line()

  def space: Doc = Text(" ")

  def empty: Doc = Empty
  def nest(level: Int, doc: Doc): Doc = Nest(level, doc)
  def setLayout(layout: Layout, doc: Doc): Doc = SetLayout(layout, doc)
  def layoutChoice(singleLine: Doc, multiLine: Doc): Doc = LayoutChoice(singleLine, multiLine)

  // ── Rendering ────────────────────────────────────────────────────

  /** Pretty-print a document. The top-level layout defaults to MultiLine. */
  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, Layout.MultiLine, doc)))
    sb.toString()
  }

  /** Pretty-print a document with an explicit top-level layout. */
  def pretty(layout: Layout, doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, layout, doc)))
    sb.toString()
  }

  @tailrec
  private def render(sb: StringBuilder, k: Int, docs: List[(Int, Layout, Doc)]): Unit = docs match {
    case Nil => ()

    case (_, _, Empty) :: z =>
      render(sb, k, z)

    case (i, l, Concat(x, y)) :: z =>
      render(sb, k, (i, l, x) :: (i, l, y) :: z)

    case (i, l, Nest(j, x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)       // no-op
        case Layout.MultiLine  => render(sb, k, (i + j, l, x) :: z)   // indent
      }

    case (_, _, Text(s)) :: z =>
      sb.append(s)
      render(sb, k + s.length, z)

    case (i, l, Line()) :: z =>
      l match {
        case Layout.SingleLine =>
          sb.append(' ')
          render(sb, k + 1, z)
        case Layout.MultiLine =>
          sb.append('\n')
          sb.append(" " * i)
          render(sb, i, z)
      }

    case (i, l, LayoutChoice(s, m)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, s) :: z)
        case Layout.MultiLine  => render(sb, k, (i, l, m) :: z)
      }

    case (i, _, SetLayout(newLayout, x)) :: z =>
      render(sb, k, (i, newLayout, x) :: z)
  }

  // ── Utility combinators ──────────────────────────────────────────

  /** Fold a list of documents using the given binary operator. */
  def folddoc(f: (Doc, Doc) => Doc, docs: List[Doc]): Doc = docs match {
    case Nil      => Empty
    case x :: Nil => x
    case x :: xs  => f(x, folddoc(f, xs))
  }

  /** Concatenate documents horizontally with spaces. */
  def spread(docs: List[Doc]): Doc = folddoc(_ <+> _, docs)

  /** Concatenate documents vertically with line breaks. */
  def stack(docs: List[Doc]): Doc = folddoc(_ <|> _, docs)

  /** Separate documents with a given separator. */
  def sep(separator: Doc, docs: List[Doc]): Doc = folddoc((a, b) => a <> separator <> b, docs)

  /** Wrap content in brackets with nesting:
    * `text(l) <> nest(2, line <> x) <> line <> text(r)` */
  def bracket(l: String, x: Doc, r: String): Doc =
    text(l) <> nest(2, line <> x) <> line <> text(r)
}
