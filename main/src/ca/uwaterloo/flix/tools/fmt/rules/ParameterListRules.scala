package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.TokenKind.{ArrowThickR, ArrowThinRTight, ArrowThinRWhitespace, Backslash, Colon, Comma}
import ca.uwaterloo.flix.tools.fmt.Doc
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, space, text}

object ParameterListRules extends FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc] = {
    case tree if tree.kind == TreeKind.ParameterList => formatParameterList(tree)
  }

  private def formatParameterList(tree: SyntaxTree.Tree): Doc = {
    if (tree.loc.isSingleLine) formatSingleLineParameterList(tree)
    else reconstruct(tree)
  }

  private def formatSingleLineParameterList(tree: SyntaxTree.Tree): Doc = {
    val tokens = collectTokens(tree)

    tokens match {
      case Nil => empty
      case head :: tail =>
        tail.foldLeft((text(head.text), head: Token)) {
          case ((accDoc, prev), token) =>
            val gap = spaceBetween(prev, token)
            (accDoc <> gap <> text(token.text), token)
        }._1
    }
  }

  private def spaceBetween(prev: Token, curr: Token): Doc =
    (prev.kind, curr.kind) match {
      case (_, Comma | Colon)   => empty
      case (Comma | Colon, _)   => space
      case (Backslash, _) => space
      case (_, ArrowThinRWhitespace) => space
      case (ArrowThinRWhitespace, _) => space
      case (_, ArrowThinRTight) => space
      case (ArrowThinRTight, _) => space
      case (_, ArrowThickR) => space
      case (ArrowThickR, _) => space
      case (_, Backslash) => space
      case _                    => empty
    }
}
