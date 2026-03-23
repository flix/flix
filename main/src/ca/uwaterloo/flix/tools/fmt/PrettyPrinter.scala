package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, line, pretty, space, text}
import ca.uwaterloo.flix.tools.fmt.rules.FormatterModule

/**
  * TODO: Think about nesting like Binary expression?
  */
object PrettyPrinter {

  /**
    * The complete rule registry, composed of all [[FormatterModule]] instances
    * declared in [[FormatterRegistry]].
    */
  private val rules: PartialFunction[SyntaxTree.Tree, Doc] =
    FormatterRegistry.modules
      .map(_.rules)
      .reduce(_ orElse _)
  /**
    * Formats the given syntax tree into a string by applying predefined formatting rules.
    * The method traverses the class hierarchy of the trees kind to find the most specific
    * applicable formatting rule. If no specific rule is found we fallback to preserving
    * the original formatting.
    *
    * @param tree the syntax tree to format
    * @return a string representing the formatted source code of the given syntax tree
    */
  def format(tree: SyntaxTree.Tree): String =
    pretty(treeToDoc(tree))

  /**
    * Transforms a syntax tree into a document by applying the most specific formatting rule
    * available for the tree's kind. If no specific rule is found, it reconstructs the document.
    * @param tree the syntax tree to transform into a document
    * @return a document representing the formatted structure of the given syntax tree
    */
  private[tools] def treeToDoc(tree: SyntaxTree.Tree): Doc =
    rules.lift(tree).getOrElse(reconstruct(tree))

  private[tools] def reconstruct(tree: SyntaxTree.Tree): Doc = {
    val (doc, _) = tree.children.foldLeft((empty, Option.empty[Token])) {
      case ((acc, p), token: Token) =>
        val gap = p.map(gapBetween(_, token)).getOrElse(empty)
        (acc <> gap <> text(token.text), Some(token))

      case ((acc, p), child: SyntaxTree.Tree) =>
        val firstToken = collectTokens(child).headOption
        val gap = (p, firstToken) match {
          case (Some(prev), Some(f)) => gapBetween(prev, f)
          case _                     => empty
        }
        val childDoc  = gap <> treeToDoc(child)
        val lastToken = collectTokens(child).lastOption
        (acc <> childDoc, lastToken orElse p)

      case (state, _) =>
        state
    }
    doc
  }

  private def gapBetween(prev: Token, curr: Token): Doc = {
    val gap      = prev.src.data.slice(prev.endIndex, curr.startIndex)
    val newlines = gap.count(_ == '\n')
    val spaces   = gap.reverseIterator.takeWhile(_ == ' ').length

    Iterator.fill(newlines)(line).foldLeft(empty)(_ <> _) <>
      Iterator.fill(spaces)(space).foldLeft(empty)(_ <> _)
  }

  private[tools] def collectTokens(tree: SyntaxTree.Tree): List[Token] =
    tree.children.toList.flatMap {
      case t: Token           => List(t)
      case c: SyntaxTree.Tree => collectTokens(c)
    }
}
