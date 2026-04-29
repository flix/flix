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
  * The [[Layout]] trait represents the layout mode for rendering documents.
  * It is either `SingleLine` for rendering documents in a single line
  * or `MultiLine` for rendering documents across multiple lines.
  */
sealed trait Layout
object Layout {
  case object SingleLine extends Layout
  case object MultiLine extends Layout
}

/**
  * The `Doc` trait represents a document from the Wadler prettier printer paper.
  * It provides combinators for constructing a document tree and a rendering function to
  * transform the document into a "pretty" string representation based on the formatting rules defined.
  *
  * - `<>` is the concatenation operator that combines two documents without any space.
  * - `<+>` is the concatenation operator that combines two documents with a single space in between.
  * - `<|>` is the concatenation operator that combines two documents with a line break in between.
  */
sealed trait Doc {
  def <>(right: Doc): Doc = Doc.Concat(this, right)
  def <+>(right: Doc): Doc = this <> Doc.space <> right
  def <|>(right: Doc): Doc = this <> Doc.line <> right
}

/**
  * The `Doc` object provides methods for constructing documents and a pretty printing function to render the document as a string.
  * It defines various document combinators defined in the Wadler paper.
  * The `render` function is a function that processes the document tree and produces a string representation based on the layout rules.
  *
  * - `Empty` represents an empty document.
  * - `Text` represents a document containing a string.
  * - `Concat` represents the concatenation of two documents.
  * - `Line` represents a line break that can be rendered as a space or a newline depending on the layout mode.
  * - `HardLine` represents a hard line break that is always rendered as a newline regardless of the layout mode.
  * - `Nest` represents a nested document that increases the indentation level for its content.
  * - `Align` represents a document that aligns its content to the current indentation level.
  * - `LayoutChoice` represents a choice between two documents based on the layout mode.
  * - `SetLayout` represents a document that sets a specific layout mode for its content.
  */
object Doc {

  case object Empty extends Doc
  case class Text(str: String) extends Doc
  case class Concat(left: Doc, right: Doc) extends Doc
  case object Line extends Doc
  case object HardLine extends Doc
  case class Nest(level: Int, doc: Doc) extends Doc
  case class Align(doc: Doc) extends Doc
  case class LayoutChoice(singleLine: Doc, multiLine: Doc) extends Doc
  case class SetLayout(layout: Layout, doc: Doc) extends Doc

  def text(str: String): Doc = Text(str)
  def line: Doc = Line
  def hardline: Doc = HardLine
  def space: Doc = Text(" ")
  def empty: Doc = Empty
  def nest(level: Int, doc: Doc): Doc = Nest(level, doc)
  def align(doc: Doc): Doc = Align(doc)
  def setLayout(layout: Layout, doc: Doc): Doc = SetLayout(layout, doc)
  def layoutChoice(singleLine: Doc, multiLine: Doc): Doc = LayoutChoice(singleLine, multiLine)

  /**
    * The `pretty` function takes a `Doc` and produces a string representation of the document using the default layout mode of `MultiLine`.
    *
    * @param doc the document to be rendered as a string
    * @return a string representation of the document based on the default layout mode of `MultiLine`
    */
  def pretty(doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, Layout.MultiLine, doc)))
    sb.toString()
  }

  /**
    * The `pretty` function takes a `Layout` and a `Doc` and produces a string representation of the document.
    *
    * @param layout the layout mode to use for rendering the document, either `SingleLine` or `MultiLine`
    * @param doc the document to be rendered as a string
    * @return a string representation of the document based on the specified layout mode
    */
  def pretty(layout: Layout, doc: Doc): String = {
    val sb = new StringBuilder
    render(sb, 0, List((0, layout, doc)))
    sb.toString()
  }

  /**
    * The `render` function is tail recursive and processes the document tree.
    * It takes a `StringBuilder` to accumulate the output and an integer `k` representing the current indentation level.
    * The `docs` parameter is a list of tuples where each tuple contains the current indentation level, the layout mode
    * and the document to be rendered.
    *
    * @param sb the StringBuilder to accumulate the output
    * @param k the current indentation level
    * @param docs the list of documents to be rendered, each with its own indentation level and layout mode
    */
  private def trimTrailing(sb: StringBuilder): Unit = {
    var i = sb.length - 1
    while (i >= 0 && sb.charAt(i) == ' ') i -= 1
    sb.setLength(i + 1)
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
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine  => render(sb, k, (i + j, l, x) :: z)
      }

    case (i, l, Align(x)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, x) :: z)
        case Layout.MultiLine  => render(sb, k, (k, l, x) :: z)
      }

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

    case (i, l, LayoutChoice(s, m)) :: z =>
      l match {
        case Layout.SingleLine => render(sb, k, (i, l, s) :: z)
        case Layout.MultiLine  => render(sb, k, (i, l, m) :: z)
      }

    case (i, _, SetLayout(newLayout, x)) :: z =>
      render(sb, k, (i, newLayout, x) :: z)
  }

  def stack(docs: List[Doc]): Doc = folddoc(_ <|> _, docs)
  def hardStack(docs: List[Doc]): Doc = folddoc((a, b) => a <> hardline <> b, docs)
  def sep(separator: Doc, docs: List[Doc]): Doc = folddoc((a, b) => a <> separator <> b, docs)
  private def folddoc(f: (Doc, Doc) => Doc, docs: List[Doc]): Doc = docs match {
    case Nil      => Empty
    case x :: Nil => x
    case x :: xs  => f(x, folddoc(f, xs))
  }
}
