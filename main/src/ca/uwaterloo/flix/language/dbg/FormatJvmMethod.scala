package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.TypedAst

object FormatJvmMethod {

  /**
    * JvmMethod.
    */
  def format(method: TypedAst.JvmMethod): String = method match {
    case TypedAst.JvmMethod(ident, fparams, exp, _, _, _, _) =>
      s"JvmMethod($ident, ${fparams.map(FormatFormalParam.format).mkString("(", ", ", ")")}, ${FormatExpression.format(exp)})"
  }

}
