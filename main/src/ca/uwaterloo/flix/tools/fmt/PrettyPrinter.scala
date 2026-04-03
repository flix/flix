package ca.uwaterloo.flix.tools.fmt

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token, TokenKind}
import ca.uwaterloo.flix.language.ast.SyntaxTree.{Tree, TreeKind}
import ca.uwaterloo.flix.tools.fmt.Doc.*

object PrettyPrinter {

  def format(tree: Tree): String = {
    val doc = traverse(tree)
    println("Generated Doc:")
    println(doc)
    Doc.pretty(doc)
  }

  def traverse(tree: Tree): Doc = tree.kind match {
    case TreeKind.Expr.Binary   => prettyBinary(tree)
    case TreeKind.ParameterList => prettyParameterList(tree)
    case TreeKind.Parameter     => prettyParameter(tree)
    case _                      => prettyFallback(tree)
  }

  private def prettyParameterList(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Comma =>
        acc <> text(",") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  private def prettyParameter(tree: Tree): Doc = {
    tree.children.foldLeft(empty) {
      case (acc, token: Token) if token.kind == TokenKind.Colon =>
        acc <> text(":") <> space
      case (acc, child) =>
        acc <> prettyChild(child)
    }
  }

  private def prettyBinary(tree: Tree): Doc = {
    val parts = tree.children.map(prettyChild)
    parts.reduceLeftOption(_ <+> _).getOrElse(empty)
  }

  private def prettyFallback(tree: Tree): Doc = {
    tree.children.map(prettyChild).reduceLeftOption(_ <> _).getOrElse(empty)
  }

  private def prettyChild(child: SyntaxTree.Child): Doc = child match {
    case token: Token => text(token.text)
    case tree: Tree   => traverse(tree)
  }
}
