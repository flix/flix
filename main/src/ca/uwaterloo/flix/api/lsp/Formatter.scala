package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Expr.Binary

/**
  * A pretty-printer for Flix syntax trees.
  */
object Formatter {

  /**
    * Pretty prints the given syntax tree root.
    *
    * @param root the syntax tree root
    * @param uri the file path of the syntax tree
    * @return a pretty-printed string representation of the syntax tree
    */
  def format(root: SyntaxTree.Root, uri: String): List[TextEdit] =
    findTreeBasedOnUri(root, uri).map(traverseTree).getOrElse(Nil)

  private def traverseTree(tree: SyntaxTree.Tree): List[TextEdit] = {
    val editsHere = tree.kind match {
      case Binary if tree.loc.isSingleLine => handleBinaryExpr(tree)
      case _ => Nil
    }

    val editsFromChildren = tree.children.flatMap {
      case childTree: SyntaxTree.Tree => traverseTree(childTree)
      case _                          => Nil
    }
    editsHere ++ editsFromChildren
  }

  private def handleBinaryExpr(tree: SyntaxTree.Tree): List[TextEdit] = {
    val tokens: Array[Token] = collectTokens(tree)
    val tokenStrings: Array[String] = tokens.map(_.text)

    val formattedString = tokenStrings.mkString(" ")
    val startPos = Position(tree.loc.beginLine, tree.loc.beginCol)
    val endPos = Position(tree.loc.endLine, tree.loc.endCol)
    val range = Range(startPos, endPos)

    List(TextEdit(range, formattedString))
  }

  private def collectTokens(child: SyntaxTree.Child): Array[Token] = child match {
    case token: Token => Array(token)
    case tree: SyntaxTree.Tree =>
      tree.children.flatMap(collectTokens)
  }

  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    root.units.collectFirst { case (source, tree) if source.name == uri => tree }
  }

}
