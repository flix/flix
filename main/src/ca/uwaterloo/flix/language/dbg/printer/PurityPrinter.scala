package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Purity
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Eff

object PurityPrinter {

  /**
    * Returns the [[DocAst.Eff]] representation of `purity`.
    */
  def print(purity: Purity): Eff = purity match {
    case Purity.Pure => Eff.Pure
    case Purity.Impure => Eff.Impure
  }

}
