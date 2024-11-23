package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.{Decl, Expr, Pattern, Predicate, Type, UsesOrImports}
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.MapOps

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
      DocAst.Expr.AsIs("Err")
    case other =>
      DocAst.Expr.AsIs(other.toString)
  }

  private def print(child: SyntaxTree.Child): DocAst.Expr = child match {
    case token: Token => print(token)
    case tree: SyntaxTree.Tree => print(tree)
    case _ => DocAst.Expr.Unknown
  }

}
