package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

object FormatFormalParam {

  /**
    * TypedAst.
    */
  def format(f0: TypedAst.FormalParam): String = f0 match {
    case TypedAst.FormalParam(sym, _, _, _, _) => sym.toString
  }

}
