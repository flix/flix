package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.tools.Doc.{empty, lineBreak, pretty, space, text}

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
    val doc = treeToDoc(tree)
    pretty(doc)
  }

  /**
    * Transforms a syntax tree into a Doc by collecting the tokens and reconstructing the original source code.
    * @param tree
    * @return
    */
  private def treeToDoc(tree: SyntaxTree.Tree): Doc =
    treeToDocWithLast(tree, None)._1

  /**
    * Transforms a syntax tree into a Doc by collecting the tokens and reconstructing the original source code.
    * It keeps track of the last token encountered to compute the [[gapDoc]] between tokens.
    * @param tree
    * @param prevToken
    * @return
    */
  private def treeToDocWithLast(tree: SyntaxTree.Tree, prevToken: Option[Token]): (Doc, Option[Token]) =
    tree.kind match {
      case TreeKind.UsesOrImports.UseOrImportList =>
        (reorderImportsToDoc(tree), collectTokens(tree).lastOption)
      case _ =>
        tree.children.toList.foldLeft((empty, prevToken)) {
          case ((acc, prev), token: Token) =>
            val gap = prev.map(gapDoc(_, token)).getOrElse(empty)
            (acc <> gap <> text(token.text), Some(token))
          case ((acc, prev), child: SyntaxTree.Tree) =>
            val (childDoc, lastToken) = treeToDocWithLast(child, prev)
            (acc <> childDoc, lastToken)
          case ((acc, prev), _) =>
            (acc, prev)
        }
    }

  private def collectTokens(tree: SyntaxTree.Tree): List[Token] =
    tree.children.toList.flatMap {
      case token: Token            => List(token)
      case child: SyntaxTree.Tree  => collectTokens(child)
    }

  /**
    * gapDoc computes the whitespace and line breaks between two tokens by slicing the original source code between
    * the end of the previous token and the start of the current token.
    * In addition, it counts the number of line breaks and spaces in the gap.
    *
    * @param prev the previous token
    * @param curr the current token
    * @return a Doc representing the whitespace and line breaks between the two tokens
    */
  private def gapDoc(prev: Token, curr: Token): Doc = {
    val gap = prev.src.data.slice(prev.endIndex, curr.startIndex)

    val newlines = gap.count(_ == '\n')
    val spaces   = gap.reverseIterator.takeWhile(_ == ' ').length

    Iterator.fill(newlines)(lineBreak("\n")).foldLeft(empty)(_ <> _) <>
    Iterator.fill(spaces)(space).foldLeft(empty)(_ <> _)
  }

  /**
    * Reorder the import and use statements given a syntax tree representing a use or import list.
    * The sorting logic is the alphabetical order of the full import path.
    *
    * @param tree the syntax tree representing a use or import list
    * @return a Doc representing the sorted use and import statements
    */
  private def reorderImportsToDoc(tree: SyntaxTree.Tree): Doc = {
    val importTrees = tree.children.toList.collect {
      case t: SyntaxTree.Tree if t.kind == TreeKind.UsesOrImports.Use || t.kind == TreeKind.UsesOrImports.UseMany || t.kind == TreeKind.UsesOrImports.Import => t
    }

    val (uses, imports) = importTrees.partition(t =>
      t.kind == TreeKind.UsesOrImports.Use || t.kind == TreeKind.UsesOrImports.UseMany
    )

    val sortedUses    = uses.sortBy(importSortKey)
    val sortedImports = imports.sortBy(importSortKey)

    val allSorted = sortedUses ++ sortedImports

    allSorted.foldLeft(empty) { (acc, t) =>
      acc <> treeToDoc(t) <> lineBreak("\n")
    }
  }

  /**
    * Sort by alphabetical order of the full import path
    * @param tree the syntax tree representing a use or import statement
    * @return returns the full import path as a string
    */
  private def importSortKey(tree: SyntaxTree.Tree): String = {
    val tokens = collectTokens(tree)
    val fullName = tokens.map(_.text).mkString("")
    fullName
  }
}
