package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.Ast

object FormatTypeConstraint {

  /**
    * Formats the given `tconstr` as `Class[Param]`.
    */
  def formatTypeConstraint(tconstr: Ast.TypeConstraint)(implicit audience: Audience): String = tconstr match {
    case Ast.TypeConstraint(sym, arg, _) =>
      val typeString = FormatType.formatWellKindedType(arg)
      s"${sym}[${typeString}]"
  }
}
