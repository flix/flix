package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.tools.Doc.{pretty, empty, text, space, lineBreak}

object PrettyPrinter {

  def format(tree: SyntaxTree.Tree): String = {
    val tokens = collectTokens(tree)
    val doc    = tokensToDoc(tokens)
    pretty(doc)
  }

  private def collectTokens(tree: SyntaxTree.Tree): List[Token] =
    tree.children.toList.flatMap {
      case token: Token            => List(token)
      case child: SyntaxTree.Tree  => collectTokens(child)
    }

  private def tokensToDoc(tokens: List[Token]): Doc =
    tokens match {
      case Nil => empty
      case _ =>
        tokens.sliding(2).foldLeft(text(tokens.head.text)) {
          case (doc, List(prev, curr)) => doc <> gapDoc(prev, curr) <> text(curr.text)
          case (doc, _)                => doc
        }
    }

  private def gapDoc(prev: Token, curr: Token): Doc = {
    val gap       = prev.src.data.slice(prev.endIndex, curr.startIndex)
    val newlines  = gap.count(_ == '\n')
    val spaces    = gap.reverse.takeWhile(_ == ' ').length

    if (newlines > 0)
      List.fill(newlines)(lineBreak("\n")).foldLeft(empty)(_ <> _) <>
        List.fill(spaces)(space).foldLeft(empty)(_ <> _)
    else
      List.fill(spaces)(space).foldLeft(empty)(_ <> _)
  }
}
