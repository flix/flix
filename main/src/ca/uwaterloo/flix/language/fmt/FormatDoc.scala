package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.Ast

object FormatDoc {

  /**
    * Returns a markdown string for the given documentation `doc`.
    */
  def asMarkDown(doc: Ast.Doc): String = doc.lines.mkString("\n")

}
