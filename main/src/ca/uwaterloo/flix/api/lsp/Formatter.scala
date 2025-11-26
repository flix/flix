package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Expr.Binary
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Operator

import scala.collection.mutable.ListBuffer

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
      case Binary if tree.loc.isSingleLine => formatBinaryExpression(tree)
      case _                               => Nil
    }

    val editsFromChildren = tree.children.flatMap {
      case childTree: SyntaxTree.Tree => traverseTree(childTree)
      case _                          => Nil
    }
    editsHere ++ editsFromChildren
  }

  private def formatBinaryExpression(tree: SyntaxTree.Tree): List[TextEdit] = {
    val tokens = collectTokens(tree)
    val edits = ListBuffer.empty[TextEdit]

    val operatorTrees = findOperatorTrees(tree)
    val operatorTokens = operatorTrees.flatMap(opTree => collectTokens(opTree))

    for (op <- operatorTokens) {
      val index = tokens.indexOf(op)
      if (index > 0 && index < tokens.length - 1) {
        val left  = tokens(index - 1)
        val right = tokens(index + 1)

        val beforeRange = Range(
          Position(left.sp2.lineOneIndexed, left.sp2.colOneIndexed),
          Position(op.sp1.lineOneIndexed,   op.sp1.colOneIndexed)
        )
        val afterRange = Range(
          Position(op.sp2.lineOneIndexed,   op.sp2.colOneIndexed),
          Position(right.sp1.lineOneIndexed, right.sp1.colOneIndexed)
        )

        edits += TextEdit(beforeRange, " ")
        edits += TextEdit(afterRange, " ")
      }
    }
    edits.toList
  }

  private def findOperatorTrees(tree: SyntaxTree.Tree): List[SyntaxTree.Tree] =
    tree.children.flatMap {
      case t: SyntaxTree.Tree =>
        if (t.kind == Operator) t :: findOperatorTrees(t) else findOperatorTrees(t)
      case _ => Nil
    }.toList


  private def collectTokens(child: SyntaxTree.Child): Array[Token] = child match {
    case token: Token => Array(token)
    case tree: SyntaxTree.Tree =>
      tree.children.flatMap(collectTokens)
  }

  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    root.units.collectFirst { case (source, tree) if source.name == uri => tree }
  }

}
