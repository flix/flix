package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Purity
import ca.uwaterloo.flix.language.dbg.DocAst.Type

object PurityPrinter {

  /** Returns the [[Type]] representation of `purity`. */
  def print(purity: Purity): Type = purity match {
    case Purity.Pure => Type.Pure
    case Purity.Impure => Type.Impure
    case Purity.ControlImpure => Type.ControlImpure
  }

}
