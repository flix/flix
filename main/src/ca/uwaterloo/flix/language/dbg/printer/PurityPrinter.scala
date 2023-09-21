package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Purity
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Effect

object PurityPrinter {

  /**
    * Returns the [[DocAst.Effect]] representation of `purity`.
    */
  def print(purity: Purity): Effect = purity match {
    case Purity.Pure => Effect.Pure
    case Purity.Impure => Effect.Impure
  }

}
