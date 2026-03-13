package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.tools.Doc.{pretty, empty, text, space, lineBreak}

/**
  * A pretty printer for the Flix source code.
  * It traverses the [[SyntaxTree]] to collects [[Token]] and reconstructs the original source code by preserving the
  * whitespace and formatting between [[Token]].
  */
object PrettyPrinter {

  /**
    * Formats the given syntax tree into a string by collecting the tokens and reconstructing the original source code.
    * @param tree the syntax tree to format
    * @return the formatted string representation of the syntax tree
    */
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
    val gap = prev.src.data.slice(prev.endIndex, curr.startIndex)

    val newlines = gap.count(_ == '\n')
    val spaces   = gap.reverseIterator.takeWhile(_ == ' ').length

    Iterator.fill(newlines)(lineBreak("\n")).foldLeft(empty)(_ <> _) <>
    Iterator.fill(spaces)(space).foldLeft(empty)(_ <> _)
  }
}
