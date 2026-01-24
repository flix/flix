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

    println(highlight(p, Formatter.getDefault))
  }

  def highlight(p: String, formatter: Formatter): String = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted

    // Lex source to get ALL tokens (root.tokens only has keywords/modifiers/comments)
    val source = Source.fromString(Input.Unknown, p)
    val (allTokens, _) = Lexer.lex(source)

    val flix = new Flix().addVirtualPath(VirtualPath, p)
    val (optRoot, _) = flix.check()

    optRoot match {
      case Some(root) =>
        implicit val r: TypedAst.Root = root
        val semanticTokens = SemanticTokensProvider.getSemanticTokens(VirtualPath.toString)
        applyColors(p, allTokens, semanticTokens, formatter)

      case None =>
        p // Compilation failed - return unchanged
    }
  }

  private def applyColors(source: String, tokens: Array[Token],
    semanticTokens: List[SemanticToken],
    formatter: Formatter): String = {
    // Build map: (line, col) -> SemanticTokenType using SemanticToken.loc directly
    val tokenMap: Map[(Int, Int), SemanticTokenType] = semanticTokens.map { t =>
      ((t.loc.startLine, t.loc.startCol), t.tpe)
    }.toMap

    val sb = new StringBuilder
    var i = 0
    for (token <- tokens.sortBy(_.startIndex) if token.kind != TokenKind.Eof) {
      if (token.startIndex > i) {
        sb.append(source.substring(i, token.startIndex))
      }
      val text = token.text
      val key = (token.start.lineOneIndexed.toInt, token.start.colOneIndexed.toInt)
      tokenMap.get(key) match {
        case Some(tpe) => sb.append(colorize(text, tpe))
        case None => sb.append(text)
      }
      i = token.endIndex
    }
    if (i < source.length) sb.append(source.substring(i))
    sb.toString()
  }


  /**
    * Returns the text wrapped in ANSI escape codes for the given semantic token type.
    */
  private def colorize(text: String, tpe: SemanticTokenType): String = {
    tokenColor(tpe) match {
      case Some((r, g, b)) => s"\u001b[38;2;$r;$g;${b}m$text\u001b[0m"
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
