package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.tools.fmt.Doc
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, text}


object BinaryExpressionRules extends FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc] = {
    case tree if tree.kind == TreeKind.Expr.Binary => formatBinary(tree)
  }

  private def formatBinary(tree: SyntaxTree.Tree): Doc =
    if (tree.loc.isSingleLine)
      formatSingleLineBinary(tree)
    else
      reconstruct(tree)

  private def formatSingleLineBinary(tree: SyntaxTree.Tree): Doc = {
    val childDocs = tree.children.toList.collect {
      case child: SyntaxTree.Tree                     => treeToDoc(child)
      case token: ca.uwaterloo.flix.language.ast.Token => text(token.text)
    }

    childDocs match {
      case Nil          => empty
      case head :: tail => tail.foldLeft(head)(_ <+> _)
    }
  }

}
