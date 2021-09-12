package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{Entity, Index}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {
  /**
   * The token types currently supported by the provider.
   *
   * Note: The intermediate TypeScript server is responsible for communicating the legend
   * to the client. The TS server must list the token types in the same order as is used
   * here in the Scala code. The reason for this is that the response is encoded as a
   * sequence of integers, and the integer representing a token type is the type's
   * index in the legend.
   */
  private object TokenType extends Enumeration {
    type TokenType = Value

    val Number, Str = Value
  }

  /**
   * The token modifiers currently supported by the provider.
   *
   * Note: See the remark in the documentation for SemanticTokenType; the same
   * constraint applies to modifiers: The TS server must list the modifiers
   * in the same order as is used here in the Scala code.
   */
  private object TokenModifier extends Enumeration {
    type TokenModifier = Value

    val Declaration, Definition = Value
  }

  private case class SemanticToken(loc: SourceLocation,
                                   tokenType: SemanticTokensProvider.TokenType.TokenType,
                                   tokenModifiers: List[SemanticTokensProvider.TokenModifier.TokenModifier])

  def provideSemanticTokens(uri: String)(implicit index: Index, root: Root): JObject = {
    val entities = index.query(uri)
    val semanticTokens = entities.flatMap(getSemanticTokens)
    val encoding = encodeSemanticTokens(semanticTokens)
    val result = ("data" -> encoding)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def getSemanticTokens(entity: Entity): List[SemanticToken] = entity match {
    case Entity.Exp(e) => e match {
      case Expression.Int8(_, _)
           | Expression.Int16(_, _)
           | Expression.Int32(_, _)
           | Expression.Float32(_, _)
           | Expression.Float64(_, _)
           | Expression.BigInt(_, _) => List(SemanticToken(e.loc, TokenType.Number, List()))
      case Expression.Str(_, loc) => List(SemanticToken(loc, TokenType.Str, List()))
      case _ => List() // TODO: Handle other kinds of expressions
    }
    case _ => List() // TODO: Handle other kinds of entities
  }

  // Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
  private def encodeSemanticTokens(tokens: List[SemanticToken]): List[Int] = {
    val encoding = new ArrayBuffer[Int](initialSize = 5 * tokens.size)

    var prevLine = 0
    var prevCol = 0

    for (token <- tokens.sortBy(_.loc)) {
      var relLine = token.loc.beginLine - 1
      var relCol = token.loc.beginCol - 1

      if (encoding.nonEmpty) {
        relLine -= prevLine
        if (relLine == 0) {
          relCol -= prevCol
        }
      }

      encoding += relLine
      encoding += relCol
      encoding += token.loc.endCol - token.loc.beginCol
      encoding += token.tokenType.id
      encoding += encodeModifiers(token.tokenModifiers)

      prevLine = token.loc.beginLine - 1
      prevCol = token.loc.beginCol - 1
    }

    encoding.toList
  }

  def encodeModifiers(modifiers: List[SemanticTokensProvider.TokenModifier.TokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.id))
}
