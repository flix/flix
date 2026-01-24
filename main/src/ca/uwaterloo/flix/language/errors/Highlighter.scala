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

  // Flixify Dark theme colors
  private def colorize(text: String, tpe: SemanticTokenType): String = {
    val color = tpe match {
      case SemanticTokenType.Keyword | SemanticTokenType.Modifier => (0fa, 0x3e, 0x83)
      case SemanticTokenType.Comment => (0x8c, 0x8c, 0x8c)
      case SemanticTokenType.String => (0xff, 0xee, 0x99)
      case SemanticTokenType.Number => (0xff, 0x8c, 0xf5)
      case SemanticTokenType.Regexp => (0xff, 0xee, 0x99)
      case SemanticTokenType.Function | SemanticTokenType.Method => (0xac, 0xe3, 0x40)
      case SemanticTokenType.Type | SemanticTokenType.Enum | SemanticTokenType.Interface | SemanticTokenType.Class => (0x66, 0xd9, 0xef)
      case SemanticTokenType.Variable | SemanticTokenType.Parameter | SemanticTokenType.Property => (0xc8, 0xc8, 0xc2)
      case SemanticTokenType.TypeParameter | SemanticTokenType.EnumMember => (0xf8, 0xf8, 0xf2)
      case _ => return text // no coloring
    }
    val (r, g, b) = color
    s"\u001b[38;2;$r;$g;${b}m$text\u001b[0m"
  }
}
