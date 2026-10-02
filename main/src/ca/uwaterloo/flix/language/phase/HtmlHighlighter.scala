/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.lsp.provider.SemanticTokensProvider
import ca.uwaterloo.flix.api.lsp.{SemanticToken, SemanticTokenType}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.shared.Source

/**
  * Renders the text of a source as HTML that is highlighted by its semantic tokens.
  */
object HtmlHighlighter {

  /**
    * Returns the text of `src` as a `<pre>` element.
    *
    * Every line is a `<span>` with the id `L<n>`, where `n` is its one-indexed line number,
    * followed by a newline. A token within a line is a `<span>` with the class of its type, see
    * [[cssClass]].
    *
    * The semantic tokens only cover part of the text, and some of them have no class. The text in
    * between is emitted as is, so the element always holds the entire text of `src`.
    */
  def highlight(src: Source)(implicit root: TypedAst.Root): String = {
    // The semantic tokens are split by line, i.e. no token spans more than one line.
    val tokens = SemanticTokensProvider.getSemanticTokens(src.sourceName).groupBy(_.loc.startLine)

    val sb = new StringBuilder()
    sb.append("<pre class='source-code'><code>")
    for ((line, i) <- lines(src).zipWithIndex) {
      val lineNo = i + 1
      sb.append(s"<span id='L$lineNo'>")
      highlightLine(line, tokens.getOrElse(lineNo, Nil))(sb)
      sb.append("</span>\n")
    }
    sb.append("</code></pre>")
    sb.toString()
  }

  /**
    * Returns the lines of `src`, each without its line terminator.
    *
    * A newline at the very end of the text terminates the last line, i.e. it does not begin an
    * empty one.
    */
  private def lines(src: Source): Array[String] = {
    val text = new String(src.data)
    if (text.isEmpty) {
      return Array.empty
    }
    text.stripSuffix("\n").split("\n", -1).map(_.stripSuffix("\r"))
  }

  /**
    * Appends `line` to `sb`, with each of `tokens`, which must all be on the line, in a `<span>`.
    *
    * A token that overlaps a token before it is dropped, which leaves its text in place: every
    * character of `line` is emitted exactly once, whether or not it is part of a token.
    */
  private def highlightLine(line: String, tokens: List[SemanticToken])(implicit sb: StringBuilder): Unit = {
    // The index of the first character of `line` that has not been emitted yet.
    var i = 0
    for (token <- tokens.sortBy(t => (t.loc.startCol, t.loc.endCol))) {
      // The columns are one-indexed and the end is exclusive. A token cannot be trusted to lie
      // within the line, so it is clamped to it.
      val start = Math.min(token.loc.startCol - 1, line.length)
      val end = Math.min(token.loc.endCol - 1, line.length)
      if (i <= start && start < end) {
        esc(line, i, start)
        cssClass(token.tpe) match {
          case Some(cls) =>
            sb.append(s"<span class='$cls'>")
            esc(line, start, end)
            sb.append("</span>")
          case None =>
            esc(line, start, end)
        }
        i = end
      }
    }
    esc(line, i, line.length)
  }

  /**
    * Returns the CSS class of a token of the type `tpe`, if it has one.
    *
    * The classes are defined in `highlight.css`. They are specific to the highlighter, i.e. no
    * other part of the documentation uses them, and they are short, since a page has one per token.
    *
    * A type without a class is displayed as plain text.
    */
  private def cssClass(tpe: SemanticTokenType): Option[String] = tpe match {
    case SemanticTokenType.Class => Some("ty")
    case SemanticTokenType.Comment => Some("cm")
    case SemanticTokenType.Decorator => Some("an")
    case SemanticTokenType.Effect => Some("ef")
    case SemanticTokenType.Enum => Some("ty")
    case SemanticTokenType.Function => Some("fn")
    case SemanticTokenType.Interface => Some("ty")
    case SemanticTokenType.Keyword => Some("kw")
    case SemanticTokenType.Method => Some("fn")
    case SemanticTokenType.Modifier => Some("kw")
    case SemanticTokenType.Number => Some("nu")
    case SemanticTokenType.Parameter => Some("va")
    case SemanticTokenType.Regexp => Some("st")
    case SemanticTokenType.String => Some("st")
    case SemanticTokenType.Struct => Some("ty")
    case SemanticTokenType.Type => Some("ty")
    case SemanticTokenType.TypeParameter => Some("ty")
    case SemanticTokenType.Variable => Some("va")
    case _ => None
  }

  /**
    * Appends the characters of `s` from `start` until `end` to `sb`, escaped as the content of an
    * element.
    *
    * Every other character is appended as is: unlike `xml.Utility.escape`, no character is dropped.
    */
  private def esc(s: String, start: Int, end: Int)(implicit sb: StringBuilder): Unit = {
    var i = start
    while (i < end) {
      s.charAt(i) match {
        case '&' => sb.append("&amp;")
        case '<' => sb.append("&lt;")
        case '>' => sb.append("&gt;")
        case c => sb.append(c)
      }
      i += 1
    }
  }

}
