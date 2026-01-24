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
import ca.uwaterloo.flix.language.ast.{Token, TokenKind, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.phase.Lexer
import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Paths

object Highlighter {

  // Virtual file path for the source
  private val VirtualPath = Paths.get("__highlight__.flix")

  /**
    * Returns the character offset where the given 1-indexed line starts.
    * Returns source.length if line is beyond the source.
    */
  private def lineOffset(source: String, line: Int): Int = {
    var offset = 0
    var currentLine = 1
    while (currentLine < line && offset < source.length) {
      if (source.charAt(offset) == '\n') currentLine += 1
      offset += 1
    }
    offset
  }

  def main(args: Array[String]): Unit = {
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

    println(compileAndHighlight(p, Formatter.getDefault))
  }

  def compileAndHighlight(p: String, formatter: Formatter): String = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    val flix = new Flix().addVirtualPath(VirtualPath, p)
    val (optRoot, _) = flix.check()

    optRoot match {
      case Some(root) =>
        implicit val r: TypedAst.Root = root
        implicit val f: Formatter = formatter
        val source = Source(Input.VirtualFile(VirtualPath, p, sctx), p.toCharArray)
        highlight(source, 4, 9)

      case None =>
        p // Compilation failed - return unchanged
    }
  }

  def highlight(source: Source, startLine: Int, endLine: Int)(implicit root: TypedAst.Root, formatter: Formatter): String = {
    val (allTokens, _) = Lexer.lex(source)
    val semanticTokens = SemanticTokensProvider.getSemanticTokens(source.name)
    applyColors(new String(source.data), allTokens, semanticTokens, formatter, startLine, endLine)
  }

  private def applyColors(source: String, tokens: Array[Token], semanticTokens: List[SemanticToken], formatter: Formatter, startLine: Int, endLine: Int): String = {
    // Compute substring boundaries
    val startOffset = lineOffset(source, startLine)
    val endOffset = lineOffset(source, endLine)

    // Build map for semantic tokens in range
    val tokenMap: Map[(Int, Int), SemanticTokenType] = semanticTokens
      .filter(t => t.loc.startLine >= startLine && t.loc.startLine < endLine)
      .map { t => ((t.loc.startLine, t.loc.startCol), t.tpe) }
      .toMap

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
    * Returns the text wrapped in ANSI escape codes for the given semantic token type.
    */
  private def colorize(text: String, tpe: SemanticTokenType, formatter: Formatter): String = {
    tokenColor(tpe) match {
      case Some((r, g, b)) => formatter.fgColor(r, g, b, text)
      case None => text
    }
  }

  /**
    * Returns the RGB color for the given semantic token type, if any.
    *
    * Uses Flixify Dark theme colors: https://github.com/flix/vscode-flix/blob/master/themes/flixify-dark.json
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
    case SemanticTokenType.Class => Some((102, 217, 239))
    case SemanticTokenType.Variable => Some((200, 200, 194))
    case SemanticTokenType.Parameter => Some((200, 200, 194))
    case SemanticTokenType.Property => Some((200, 200, 194))
    case SemanticTokenType.TypeParameter => Some((248, 248, 242))
    case SemanticTokenType.EnumMember => Some((248, 248, 242))
    case _ => None
  }
}
