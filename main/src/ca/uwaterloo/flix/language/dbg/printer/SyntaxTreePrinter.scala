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

  private def print(tree: SyntaxTree.Tree): DocAst.Exp = {
    val SyntaxTree.Tree(kind, children, _) = tree
    kind match {
      // ErrorTree is not a case object, so we can't rely on the generated `toString`.
      case TreeKind.ErrorTree(_) =>
        DocAst.Exp.App(DocAst.Exp.AsIs("ErrorTree"), children.iterator.map(print).toList)
      case other =>
        DocAst.Exp.App(DocAst.Exp.AsIs(other.toString), children.iterator.map(print).toList)
    }
  }

  private def print(token: Token): DocAst.Exp = token.kind match {
    // Err is not a case object, so we can't rely on the generated `toString`.
    case TokenKind.Err(_) =>
      DocAst.Exp.SquareApp(DocAst.Exp.AsIs("Err"), List(DocAst.Exp.AsIs(s"\"${token.text}\"")))
    case other =>
      DocAst.Exp.SquareApp(DocAst.Exp.AsIs(other.toString), List(DocAst.Exp.AsIs(s"\"${token.text}\"")))
  }

  private def print(child: SyntaxTree.Child): DocAst.Exp = child match {
    case token: Token => print(token)
    case tree: SyntaxTree.Tree => print(tree)
    case _ => DocAst.Exp.Unknown
  }

}
