package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.dbg.DocAst

object SyntaxTreePrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: SyntaxTree.Root): DocAst.Program = {
    val units = root.units.map{case (src, tree) => (src.name, print(tree))}.toList
    DocAst.Program(Nil, Nil, units)
  }

  private def print(tree: SyntaxTree.Tree): DocAst.Expr = {
    val SyntaxTree.Tree(kind, children, _) = tree
    kind match {
      // ErrorTree is not a case object, so we can't rely on the generated `toString`.
      case TreeKind.ErrorTree(_) =>
        DocAst.Expr.App(DocAst.Expr.AsIs("ErrorTree"), children.iterator.map(print).toList)
      case other =>
        DocAst.Expr.App(DocAst.Expr.AsIs(other.toString), children.iterator.map(print).toList)
    }
  }

  private def print(token: Token): DocAst.Expr = token.kind match {
    // Err is not a case object, so we can't rely on the generated `toString`.
    case TokenKind.Err(_) =>
      DocAst.Expr.SquareApp(DocAst.Expr.AsIs("Err"), List(DocAst.Expr.AsIs(s"\"${token.text}\"")))
    case other =>
      DocAst.Expr.SquareApp(DocAst.Expr.AsIs(other.toString), List(DocAst.Expr.AsIs(s"\"${token.text}\"")))
  }

  private def print(child: SyntaxTree.Child): DocAst.Expr = child match {
    case token: Token => print(token)
    case tree: SyntaxTree.Tree => print(tree)
    case _ => DocAst.Expr.Unknown
  }

}
