/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{SemanticToken, SemanticTokenType}
import ca.uwaterloo.flix.api.lsp.provider.SemanticTokensProvider
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition, Token, TokenKind, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.phase.Lexer
import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Paths

import scala.collection.mutable

object Highlighter {

  def main(args: Array[String]): Unit = {
    println(test01(Formatter.getDefault))
  }

  def test01(formatter: Formatter): String = {
    val p =
      """
        |/// An algebraic data type for shapes.
        |enum Shape {
        |    case Circle(Int32),          // circle radius
        |    case Square(Int32),          // side length
        |    case Rectangle(Int32, Int32) // height and width
        |}
        |
        |/// Computes the area of the given shape using
        |/// pattern matching and basic arithmetic.
        |def area(s: Shape): Int32 = match s {
        |    case Shape.Circle(r)       => 3 * (r * r)
        |    case Shape.Square(w)       => w * w
        |    case Shape.Rectangle(h, w) => h * w
        |}
        |
        |// Computes the area of a 2 by 4.
        |def main(): Unit \ IO =
        |    println(area(Shape.Rectangle(2, 4)))
        |
        |
        |""".stripMargin

    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    val VirtualPath = Paths.get("__highlight__.flix")


    val flix = new Flix().addVirtualPath(VirtualPath, p)
    val (optRoot, _) = flix.check()

    optRoot match {
      case Some(root) =>
        implicit val f: Formatter = formatter
        val source = Source(Input.VirtualFile(VirtualPath, p, sctx), p.toCharArray)

        val singleLineLoc = SourceLocation(
          isReal = true,
          source = source,
          start = SourcePosition(lineOneIndexed = 12, colOneIndexed = 10),
          end = SourcePosition(lineOneIndexed = 12, colOneIndexed = 15)
        )
        val multiLineLoc = SourceLocation(
          isReal = true,
          source = source,
          start = SourcePosition(lineOneIndexed = 4, colOneIndexed = 1),
          end = SourcePosition(lineOneIndexed = 16, colOneIndexed = 2)
        )

        // Demo 1: Single-line without syntax highlighting (root = None)
        val demo1 = highlightWithMessage("The variable 's' is unused", singleLineLoc, None)

        // Demo 2: Multi-line without syntax highlighting (root = None)
        val demo2 = highlightWithMessage("This function has unreachable code", multiLineLoc, None)

        // Demo 3: Single-line with syntax highlighting
        val demo3 = highlightWithMessage("The variable 's' is unused", singleLineLoc, Some(root))

        // Demo 4: Multi-line with syntax highlighting
        val demo4 = highlightWithMessage("This function has unreachable code", multiLineLoc, Some(root))

        s"""
           |=== Single-line (no highlighting) ===
           |$demo1
           |
           |=== Multi-line (no highlighting) ===
           |$demo2
           |
           |=== Single-line (with highlighting) ===
           |$demo3
           |
           |=== Multi-line (with highlighting) ===
           |$demo4
           |""".stripMargin

      case None =>
        p // Compilation failed - return unchanged
    }
  }

  /**
    * Returns syntax-highlighted source code with a message displayed under the given location.
    *
    * For single-line locations, shows an arrow underline with the message below.
    * For multi-line locations, shows a left-line indicator with the message at the end.
    */
  def highlightWithMessage(msg: String, loc: SourceLocation, root: Option[TypedAst.Root])(implicit formatter: Formatter): String = {
    val source = loc.source
    val sourceStr = new String(source.data)
    val coloring = root match {
      case None => Coloring.Plain
      case Some(r) =>
        val (allTokens, _) = Lexer.lex(source)
        val semanticTokens = SemanticTokensProvider.getSemanticTokens(source.name)(r)
        Coloring.Highlighted(allTokens, semanticTokens)
    }

    if (loc.startLine == loc.endLine)
      highlightSingleLine(msg, sourceStr, coloring, loc)
    else
      highlightMultiLine(msg, sourceStr, coloring, loc)
  }

  /**
    * Returns syntax-highlighted code for a single-line location with an arrow underline and message.
    *
    * Example output:
    * {{{
    * 10 | def area(s: Shape): Int32 = match s {
    *              ^^^^^
    *              The variable 's' is unused
    * }}}
    */
  private def highlightSingleLine(msg: String, source: String, coloring: Coloring, loc: SourceLocation)(implicit formatter: Formatter): String = {
    val lineNo = loc.startLine
    val lineNoStr = lineNo.toString + " | "
    val highlightedLine = coloring match {
      case Coloring.Plain => extractLine(source, lineNo)
      case Coloring.Highlighted(toks, stoks) => applyColors(source, toks, stoks, formatter, lineNo, lineNo + 1).stripLineEnd
    }

    val sb = new StringBuilder
    sb.append(formatter.fgColor(140, 140, 140, lineNoStr))
      .append(highlightedLine)
      .append(System.lineSeparator())
      .append(" " * (loc.startCol + lineNoStr.length - 1))
      .append(formatter.red("^" * (loc.endCol - loc.startCol)))
      .append(System.lineSeparator())
      .append(" " * (loc.startCol + lineNoStr.length - 1))
      .append(msg)
      .toString()
  }

  /**
    * Returns syntax-highlighted code for a multi-line location with gray line prefixes and message.
    *
    * Example output:
    * {{{
    * 10 | def area(s: Shape): Int32 = match s {
    * 11 |     case Shape.Circle(r) => 3 * (r * r)
    * 12 | }
    *
    * This function has unreachable code
    * }}}
    */
  private def highlightMultiLine(msg: String, source: String, coloring: Coloring, loc: SourceLocation)(implicit formatter: Formatter): String = {
    val numWidth = loc.endLine.toString.length
    val sb = new StringBuilder

    for (lineNo <- loc.startLine to loc.endLine) {
      val highlightedLine = coloring match {
        case Coloring.Plain => extractLine(source, lineNo)
        case Coloring.Highlighted(toks, stoks) => applyColors(source, toks, stoks, formatter, lineNo, lineNo + 1).stripLineEnd
      }
      val prefix = padLeft(numWidth, lineNo.toString) + " | "
      sb.append(formatter.fgColor(140, 140, 140, prefix))
        .append(highlightedLine)
        .append(System.lineSeparator())
    }
    sb.append(System.lineSeparator())
      .append(msg)
      .toString()
  }

  /**
    * Applies syntax highlighting colors to source code within the specified line range.
    *
    * Iterates through lexer tokens and applies colors based on semantic token information.
    * Returns the colorized source substring for the given line range.
    */
  private def applyColors(source: String, tokens: Array[Token], semanticTokens: List[SemanticToken], formatter: Formatter, startLine: Int, endLine: Int): String = {
    // Compute substring boundaries
    val startOffset = lineOffset(source, startLine)
    val endOffset = lineOffset(source, endLine)

    val tokenMap = buildTokenMap(semanticTokens, startLine, endLine)

    val sb = new StringBuilder
    var i = startOffset

    // Tokens are already sorted by position from the lexer - use index loop for early exit
    var idx = 0
    while (idx < tokens.length) {
      val token = tokens(idx)
      val tokenLine = token.start.lineOneIndexed

      if (tokenLine < startLine) {
        // Skip tokens before the range
        idx += 1
      } else if (tokenLine >= endLine || token.kind == TokenKind.Eof) {
        // Past the range or EOF - done
        idx = tokens.length
      } else {
        // Token is in range
        if (token.startIndex > i) {
          sb.append(source.substring(i, token.startIndex))
        }
        val text = token.text
        val key = (tokenLine, token.start.colOneIndexed.toInt)
        tokenMap.get(key) match {
          case Some(tpe) => sb.append(colorize(text, tpe, formatter))
          case None => sb.append(text)
        }
        i = token.endIndex
        idx += 1
      }
    }

    // Append any remaining text up to endOffset
    if (i < endOffset) sb.append(source.substring(i, endOffset))
    sb.toString()
  }

  /**
    * Builds a map from (line, column) positions to semantic token types for tokens within the given line range.
    */
  private def buildTokenMap(tokens: List[SemanticToken], startLine: Int, endLine: Int): Map[(Int, Int), SemanticTokenType] = {
    val m = mutable.Map.empty[(Int, Int), SemanticTokenType]
    for (t <- tokens) {
      if (startLine <= t.loc.startLine && t.loc.startLine < endLine) {
        m((t.loc.startLine, t.loc.startCol)) = t.tpe
      }
    }
    m.toMap
  }

  /**
    * Returns the character offset where the given 1-indexed line starts.
    * Returns source.length if line is beyond the source.
    */
  private def lineOffset(s: String, line: Int): Int = {
    var offset = 0
    var currentLine = 1
    while (currentLine < line && offset < s.length) {
      if (s.charAt(offset) == '\n') currentLine += 1
      offset += 1
    }
    offset
  }

  /**
    * Extracts a single line from the source string (1-indexed), without the trailing newline.
    */
  private def extractLine(s: String, line: Int): String = {
    val start = lineOffset(s, line)
    val end = lineOffset(s, line + 1)
    val lineContent = s.substring(start, end)
    lineContent.stripLineEnd
  }

  /**
    * Returns the text wrapped in ANSI escape codes for the given semantic token type.
    */
  private def colorize(s: String, tpe: SemanticTokenType, formatter: Formatter): String = {
    tokenColor(tpe) match {
      case None => s
      case Some((r, g, b)) => formatter.fgColor(r, g, b, s)
    }
  }

  /**
    * Returns the string `s` left-padded with spaces to the given `width`.
    */
  private def padLeft(width: Int, s: String): String =
    String.format("%" + width + "s", s)

  /**
    * Returns the RGB color for the given semantic token type, if any.
    *
    * Uses Flixify Dark theme colors: https://github.com/flix/flixify-dark/blob/master/themes/flixify-dark.json
    */
  private def tokenColor(tpe: SemanticTokenType): Option[(Int, Int, Int)] = tpe match {
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
    case SemanticTokenType.Interface => Some((102, 217, 239))
    case SemanticTokenType.Class => Some((248, 248, 242))
    case SemanticTokenType.Variable => Some((200, 200, 194))
    case SemanticTokenType.Parameter => Some((200, 200, 194))
    case SemanticTokenType.Property => Some((200, 200, 194))
    case SemanticTokenType.TypeParameter => Some((248, 248, 242))
    case SemanticTokenType.EnumMember => Some((248, 248, 242))
    case _ => None
  }

  /**
    * Represents whether syntax highlighting should be applied.
    */
  private sealed trait Coloring

  private object Coloring {
    /**
      * No syntax highlighting - render plain text.
      */
    case object Plain extends Coloring

    /**
      * Apply syntax highlighting using the provided tokens.
      */
    case class Highlighted(tokens: Array[Token], semanticTokens: List[SemanticToken]) extends Coloring
  }
}
