package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.tools.fmt.{Doc, PrettyPrinter}

trait FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc]

  protected def treeToDoc(tree: SyntaxTree.Tree): Doc =
    PrettyPrinter.treeToDoc(tree)

  protected def reconstruct(tree: SyntaxTree.Tree): Doc =
    PrettyPrinter.reconstruct(tree)

  protected def collectTokens(tree: SyntaxTree.Tree): Seq[Token] =
    PrettyPrinter.collectTokens(tree)

}
