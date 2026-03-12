package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.tools.Doc.{empty, pretty, text}


object PrettyPrinter {

  def format(tree: SyntaxTree.Tree): String = {
    val doc = treeToDoc(tree)
    pretty(doc)
  }

  private def treeToDoc(tree: SyntaxTree.Tree): Doc =
    tree.children.foldLeft(empty) { (acc, child) =>
      acc <> childToDoc(child)
    }

  private def childToDoc(child: SyntaxTree.Child): Doc = child match {
    case token: Token          => text(token.text)
    case tree: SyntaxTree.Tree => treeToDoc(tree)
  }
}
