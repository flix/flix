package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.TokenKind.{Colon, Comma}
import ca.uwaterloo.flix.tools.fmt.Doc
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, space, text}

object ParameterListRules extends FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc] = {
    case tree if tree.kind == TreeKind.ParameterList =>
      formatParameterList(tree)
  }

  private def formatParameterList(tree: SyntaxTree.Tree): Doc =
    if (tree.loc.isSingleLine)
      formatSingleLineParameterList(tree)
    else
      reconstruct(tree)

  private def formatSingleLineParameterList(tree: SyntaxTree.Tree): Doc = {
    val tokens = collectTokens(tree)

    tokens.zipWithIndex.foldLeft(empty) {
      case (acc, (token, 0)) =>
        acc <> text(token.text)

      case (acc, (token, i)) =>
        val prev = tokens(i - 1)
        val gap  = spaceBetween(prev, token)
        acc <> gap <> text(token.text)
    }
  }

  private def spaceBetween(prev: Token, curr: Token): Doc =
    (prev.kind, curr.kind) match {
      case (_, Comma | Colon)   => empty
      case (Comma | Colon, _)   => space
      case _                    => empty
    }

}
