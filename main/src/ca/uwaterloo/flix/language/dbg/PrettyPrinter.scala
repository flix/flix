package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}

object PrettyPrinter {

  /**
    * Returns a "pretty-printed" string representation of the given syntax tree `root`.
    *
    * @param root the syntax tree root
    * @return the "pretty-printed" string representation
    */
  def printPretty(root: SyntaxTree.Root): String = {
    root.units.values.map(renderTree).mkString
  }

  private def renderTree(tree: SyntaxTree.Tree): String = {
    val sb = new StringBuilder

    def visit(t: SyntaxTree.Tree): Unit = {
      val SyntaxTree.Tree(_, children, _) = t
      children.foreach {
        case token: Token => sb.append(token.text)
        case childTree: SyntaxTree.Tree => visit(childTree)
        case _ => ()
      }
    }

    visit(tree)
    sb.toString()
  }
}
